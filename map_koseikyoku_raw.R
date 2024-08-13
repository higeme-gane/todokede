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
    slice(slice_num) |> 
    mutate(full_link = str_c(base_url, each_link))
}) |> list_rbind()

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
  mutate(across(c(last_modified, date), dmy_hms))

# ダウンロードディレクトリを設定する
download_dir <- "downloads"
if (!dir.exists(download_dir)) {
  dir.create(download_dir)
}
# 該当ファイルをダウンロードする
map(df_headers$url, ~{
  file_name <- basename(.x)
  file_path <- file.path(download_dir, file_name)
  download.file(.x, file_path, mode = "wb")
})
#downloads_dirにある.zipファイル名を取得する。
df_downloads_dir <- dplyr::tibble(file_name = list.files(download_dir)) |> 
  mutate(extension = str_extract(file_name, "\\.[a-zA-Z0-9]+$")) |> 
  dplyr::filter(extension == ".zip")

# 元のカレントディレクトリを保存
original_dir <- getwd()
# "downloads"ディレクトリに移動する
setwd(download_dir)
# downloadディレクトリの絶対パスを取得
download_dir <- getwd()
# zipディレクトリを設定し、zipファイルのみzipディレクトリに移動
zip_dir <- "zips"
if (!dir.exists(zip_dir)) {
  dir.create(zip_dir)
}
file_move(df_downloads_dir$file_name, zip_dir)
# zipディレクトリの絶対パスを取得
setwd(zip_dir)
zip_dir <- getwd()
df_downloads_dir <- mutate(.data = df_downloads_dir,
                           zip_file_name = str_c(zip_dir, "/", file_name))
# システムコマンドを使って解凍する
map(df_downloads_dir$zip_file_name, ~{
  zip_command <- paste("unzip", shQuote(.x), "-d", shQuote(download_dir))
  system(zip_command)
})

setwd(download_dir)
sub_dirs <- dir_ls(download_dir, recurse = FALSE, type = "directory")
# 各ディレクトリ内の.xlsxファイルを探して移動
map(sub_dirs, ~{
  df_dir_work <- dplyr::tibble(full_path = dir_ls(.x)) |>
  mutate(extension = str_extract(full_path, "\\.[a-zA-Z0-9]+$")) |> 
  dplyr::filter(extension == ".xlsx")
file_move(df_dir_work$full_path, download_dir)
dir_delete(.x)
})
#九州厚生局のファイルには歯科、薬局の施設基準も含まれているため除外
df_shika_yakkyoku <- dplyr::tibble(file_name =  list.files(download_dir)) |> 
  dplyr::filter(str_detect(file_name, c("shika")) |
                str_detect(file_name, c("yakkyoku"))) |> 
  mutate(full_link = str_c(download_dir, "/", file_name))
file.remove(df_shika_yakkyoku$full_link)

# 元のカレントディレクトリに戻る
setwd(original_dir)

#ダウンロードしたファイルを読み込みデータ処理
df_file <- dplyr::tibble(full_path = dir_ls("downloads")) |> 
  mutate(extension = str_extract(full_path, "\\.[a-zA-Z0-9]+$")) |> 
  dplyr::filter(extension == ".xlsx")

#47都道府県のエクセルデータを結合。
df_raw <- map(df_file$full_path, function(fn_xlsx_file) {
  sheets <- getSheetNames(fn_xlsx_file)
  map(sheets, function(fn_sheet) {
    read.xlsx(fn_xlsx_file, startRow = 4, sheet = fn_sheet) |> 
    mutate(across(everything(), as.character))
  }) |> list_rbind()
}) |> list_rbind()
#列名が日本語だと不具合を起こすことがあるので、あえてアルファベットの列名に変える。
jp_colnames <- colnames(df_raw)
en_colnames <- c("item_num","pref_code", "pref_name", "medical_density", "hosp_num",
             "sub_hosp_num", "hosp_num_3rd", "hosp_name", "hosp_zip", "hosp_address",
             "tel", "fax", "n_beds", "name", "symbol",
             "docu_num", "start_date_wareki", "na_date", "remarks", "remarks2",
             "na_code", "na_name", "class_code", "class_name")
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
         na_code, na_name, class_code, class_name)
#各都道府県の最新更新月を確認する。
piv_pref_latest <- distinct(.data = df_raw, pref_code, pref_name, start_date,
                            .keep_all = FALSE) |> 
  dplyr::filter(!is.na(start_date)) |> 
  arrange(desc(start_date)) |> 
  distinct(pref_code, .keep_all = TRUE) |> 
  arrange(pref_code) |> 
  rename(update_date = start_date)
#日本語の列名に戻す。
colnames(df_raw) <- jp_colnames

#ファイル出力(.rdsは約15MB、.csvは約300MB、.xlsxだと約80MBのファイルが出力される。)
#write.xlsx()は若干時間がかかる。
saveRDS(df_raw, str_c("rawfile_", Sys.Date(),".rds"))
# write_excel_csv(df_raw, str_c("rawfile_",Sys.Date(),".csv"))
# write.xlsx(df_raw, str_c("rawfile_",Sys.Date(),".xlsx"))

#都道府県ごとの更新日を.csv出力
write_excel_csv(piv_pref_latest, str_c("update_date_pref_", Sys.Date(), ".csv"))

#headers情報の.csv出力
write_csv(df_headers, str_c("headers", Sys.Date(), ".csv"))