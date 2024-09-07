library(conflicted)
library(tidyverse)
library(rvest)
library(httr)
library(fs)
library(openxlsx)
library(zipangu)

# ベースURLを定義する
base_url <- c("https://kouseikyoku.mhlw.go.jp")
pattern <- "https://kouseikyoku.mhlw.go.jp/([^/]+)/"

#dataダウンロード日の数値型にする(ハイフン削除のため)
date_download <- Sys.Date()
date_download <- year(date_download) * 10000 +
                 month(date_download) * 100 +
                 day(date_download)

# 各行のヘッダーを個別に抽出してデータフレーム化するモジュール
extract_headers <- function(header_list) {
  tibble(
    last_modified = header_list$`last-modified`,
    etag = header_list$etag,
    content_length = header_list$`content-length`,
    accept_ranges = header_list$`accept-ranges`,
    content_type = header_list$`content-type`,
    date = header_list$date
  )
}

df_kouseikyoku <- dplyr::tibble(
  url = c(
    "https://kouseikyoku.mhlw.go.jp/hokkaido/gyomu/gyomu/hoken_kikan/todokede_juri_ichiran.html",
    "https://kouseikyoku.mhlw.go.jp/tohoku/gyomu/gyomu/hoken_kikan/documents/201805koushin.html",
    "https://kouseikyoku.mhlw.go.jp/kantoshinetsu/chousa/kijyun.html",
    "https://kouseikyoku.mhlw.go.jp/tokaihokuriku/newpage_00349.html",
    "https://kouseikyoku.mhlw.go.jp/kinki/gyomu/gyomu/hoken_kikan/shitei_jokyo_00004.html",
    "https://kouseikyoku.mhlw.go.jp/chugokushikoku/chousaka/shisetsukijunjuri.html",
    "https://kouseikyoku.mhlw.go.jp/shikoku/gyomu/gyomu/hoken_kikan/shitei/index.html",
    "https://kouseikyoku.mhlw.go.jp/kyushu/gyomu/gyomu/hoken_kikan/index_00007.html"),
  what_num = c(rep.int(1, times = 6), 4, c("1:8"))) |>
  mutate(regions = str_extract(url, pattern)) |>
  mutate(regions = str_remove(regions, base_url)) |>
  mutate(regions = str_remove_all(regions, "^/|/$")) |> 
  mutate(extension = if_else(regions == "hokkaido",
                             true =  c(".xlsx"),
                             false =  c(".zip")))

df_links <- map(1:nrow(df_kouseikyoku), ~{
  links_url <- read_html(df_kouseikyoku$url[.x]) |> 
    html_nodes("a") |> 
    html_attr("href")
  slice_num <- eval(parse(text = df_kouseikyoku$what_num[.x]))
  dplyr::tibble(each_link = links_url) |> 
    mutate(extension = str_extract(each_link, "\\.[a-zA-Z0-9]+$")) |> 
    dplyr::filter(extension == df_kouseikyoku$extension[.x]) |> 
    slice(slice_num)
}) |> list_rbind() |> 
  mutate(full_link = str_c(base_url, each_link)) |> 
  mutate(download_file_name = basename(each_link)) |> 
  mutate(region = str_extract(each_link, "(?<=^/)[^/]+"))

df_headers_work <- map(df_links$full_link, function(fn_work){
  res <- HEAD(fn_work)
  dplyr::tibble(
    url = fn_work,
    headers = list(headers(res))
  )
}) |> list_rbind()
df_headers <- df_headers_work |> 
  mutate(headers = map(headers, extract_headers)) |> 
  unnest(headers) |> 
  mutate(content_length = as.integer(content_length)) |> 
  mutate(across(c(last_modified, date), dmy_hms)) |>
  mutate(regions = str_extract(url, pattern)) |>
  mutate(regions = str_remove(regions, base_url)) |>
  mutate(regions = str_remove_all(regions, "^/|/$")) |> 
  #九州厚生局は各県のzipファイルなので、regions列を県名に変更
  mutate(pref = gsub(".*shisetsu_([a-z]+)\\.zip", "\\1", url)) |> 
  mutate(regions = if_else(regions == "kyushu", pref, regions)) |> 
  select(-pref)

# 元のカレントディレクトリを保存
original_dir <- getwd()

#以前のデータがある場合は、更新されたデータのみダウンロードする。
latest_headers_csv <- dplyr::tibble(file_name = list.files()) |> 
  mutate(start_name = str_sub(file_name, 1, 7),
         extension = str_extract(file_name, "\\.[a-zA-Z0-9]+$")) |> 
  dplyr::filter(start_name == "headers") |> 
  dplyr::filter(extension == ".csv") |> 
  arrange(desc(file_name))
latest_raw_rds <- dplyr::tibble(file_name = list.files()) |> 
  mutate(start_name = str_sub(file_name, 1, 7),
         extension = str_extract(file_name, "\\.[a-zA-Z0-9]+$")) |> 
  dplyr::filter(start_name == "rawfile") |> 
  dplyr::filter(extension == ".rds") |> 
  arrange(desc(file_name))

if (nrow(latest_headers_csv) > 0) {
  df_before <- read_csv(latest_headers_csv$file_name[1]) |> 
  select(url, last_modified) |> 
  rename(update_date = last_modified)
} else {
  df_before <- df_headers |> 
    select(url) |> 
    mutate(update_date = ymd_hms(19010101000000))
}
df_before <- df_before |> 
  left_join(df_headers, by = "url")
# |> select(url, regions, update_date, last_modified)

df_update_regions <- df_before|> 
  mutate(across(c(update_date, last_modified), lubridate::date)) |> 
  dplyr::filter(update_date != last_modified)
vec_nrow <- nrow(df_update_regions)
vec_unupdate_regions <- dplyr::filter(.data = df_before, update_date == last_modified)$regions

if (vec_nrow == 0) {
  print("変更情報はありません。")
} else {
  df_excel_files <- map(1:vec_nrow, ~{
    #map処理の前にoriginal_dirに戻る
    setwd(original_dir)
    dir_name <- df_update_regions$regions[.x]
    download_url <- df_update_regions$url[.x]
    #dir_create関数は存在するディレクトリ名を指定した場合、
    #そのディレクトリは再作成されず、既存のディレクトリに影響を与えない
    dir_create(dir_name)
    #downloadする各regionディレクトリに移動する
    setwd(dir_name)
    # downloadディレクトリの絶対パスを取得
    download_dir <- getwd()
    #ディレクトリ内にファイルがある場合は全削除
    file_delete(list.files(download_dir))
    #ダウンロードするファイル名までのfull path(file_path)の設定
    file_name <- basename(download_url)
    file_path <- file.path(download_dir, file_name)
    #ダウンロード実行
    download.file(download_url, file_path, mode = "wb")
    #zipファイルの場合は解凍
    if (df_update_regions$content_type[.x] == "application/zip") {
      zip_command <- paste("unzip", shQuote(file_path), "-d", shQuote(download_dir))
      system(zip_command)
      #zipファイルの削除
      file_delete(file_path)
      #解凍してできたエクセルファイルをディレクトリ直下に移動
      sub_dirs <- dir_ls(download_dir, recurse = FALSE, type = "directory")
      map(sub_dirs, function(fn_dirs) {
        df_dir_work <- dplyr::tibble(full_path = dir_ls(fn_dirs)) |>
        mutate(extension = str_extract(full_path, "\\.[a-zA-Z0-9]+$")) |> 
        dplyr::filter(extension == ".xlsx")
      file_move(df_dir_work$full_path, download_dir)
        })
      #エクセルファイル以外（残ったディレクトリ）を削除
      dir_delete(sub_dirs)
    }
    #九州厚生局のファイルには歯科、薬局の施設基準も含まれているため削除
    df_shika_yakkyoku <- dplyr::tibble(file_name =  list.files(download_dir)) |> 
      dplyr::filter(str_detect(file_name, c("shika")) |
                    str_detect(file_name, c("yakkyoku"))) |> 
      mutate(full_link = file.path(download_dir, file_name))
    file_delete(df_shika_yakkyoku$full_link)
    #読み込むエクセルファイルリストの作成
    dplyr::tibble(file_name =  list.files(download_dir)) |> 
      mutate(regions = dir_name) |> 
      mutate(full_link = file.path(download_dir, file_name))
  }) |> list_rbind()
  #map処理の前にoriginal_dirに戻る
  setwd(original_dir)
  #更新された厚生局分のエクセルデータを結合。
  df_raw_update <- map(1:nrow(df_excel_files), function(fn_xlsx_file) {
    sheets <- getSheetNames(df_excel_files$full_link[fn_xlsx_file])
    map(sheets, function(fn_sheet) {
      read.xlsx(df_excel_files$full_link[fn_xlsx_file], startRow = 4, sheet = fn_sheet) |> 
      mutate(across(everything(), as.character)) |> 
      mutate(管轄厚生局 = df_excel_files$regions[fn_xlsx_file])
    }) |> list_rbind()
  }) |> list_rbind()
  if (nrow(latest_raw_rds) == 0) {
    df_raw <- df_raw_update
  } else {
    df_raw <- readRDS(latest_raw_rds$file_name[1]) |> 
      dplyr::filter(管轄厚生局 == vec_unupdate_regions) |> 
      bind_rows(df_raw_update)
  }
  #列名が日本語だと不具合を起こすことがあるので、あえてアルファベットの列名に変える。
  jp_colnames <- colnames(df_raw)
  en_colnames <- c("item_num","pref_code", "pref_name", "medical_density", "hosp_num",
              "sub_hosp_num", "hosp_num_3rd", "hosp_name", "hosp_zip", "hosp_address",
              "tel", "fax", "n_beds", "name", "symbol",
              "docu_num", "start_date_wareki", "na_date", "remarks", "remarks2",
              "na_code", "na_name", "class_code", "class_name", "regions")
  colnames(df_raw) <- en_colnames
  #和暦から西暦に変換。全てのデータをconvert_jdate()でmutate()すると時間がかかるので、
  #dintinct()で重複削除し、left_join()でdfに西暦をつける。
  df_start_date_master <- distinct(.data = df_raw, start_date_wareki,
                                  .keep_all = FALSE) |> 
    mutate(start_date = convert_jdate(str_replace_all(start_date_wareki, " ", "")))
  df_raw <- left_join(df_raw, df_start_date_master, by = "start_date_wareki") |> 
    select(-start_date_wareki) |> 
    arrange(desc(start_date)) |> 
  #都道府県、医療機関コード、施設基準が重複している場合は削除。
    distinct(pref_code, hosp_num, symbol, .keep_all = TRUE) |> 
    arrange(hosp_num) |> 
    arrange(pref_code) |>
    select(item_num, pref_code, pref_name, medical_density, hosp_num,
          sub_hosp_num, hosp_num_3rd, hosp_name, hosp_zip, hosp_address,
          tel, fax, n_beds, name, symbol,
          docu_num, start_date, na_date, remarks, remarks2,
          na_code, na_name, class_code, class_name, regions)
  #各都道府県の管轄厚生局リスト作成。
  df_kyoku_update_date <- select(.data = df_headers, regions, last_modified)
  df_pref_kyoku <- distinct(.data = df_raw, pref_code, regions, .keep_all = FALSE) |> 
    left_join(df_kyoku_update_date, by = "regions")
  #各都道府県の最新更新月を確認する。
  piv_pref_latest <- distinct(.data = df_raw, pref_code, pref_name, start_date,
                              .keep_all = FALSE) |> 
    dplyr::filter(!is.na(start_date)) |> 
    arrange(desc(start_date)) |> 
    distinct(pref_code, .keep_all = TRUE) |> 
    arrange(pref_code) |> 
    rename(latest_Filing_date = start_date) |> 
    left_join(df_pref_kyoku, by = "pref_code")
  #日本語の列名に戻す。
  colnames(df_raw) <- jp_colnames

  #ファイル出力(.rdsは約15MB、.csvは約300MB、.xlsxだと約80MBのファイルが出力される。)
  #write.xlsx()は若干時間がかかる。
  saveRDS(df_raw, str_c("rawfile_", date_download,".rds"))
  # write_excel_csv(df_raw, str_c("rawfile_",date_download,".csv"))
  # write.xlsx(df_raw, str_c("rawfile_",date_download,".xlsx"))

  #都道府県ごとの更新日を.csv出力
  write_excel_csv(piv_pref_latest, str_c("update_date_pref_", date_download, ".csv"))

  #headers情報の.csv出力
  write_csv(df_headers, str_c("headers", date_download, ".csv"))

  #downloadsディレクトリを削除する場合に実行。
  # dir_delete(download_dir)
}

