library(conflicted)
library(tidyverse)
library(rvest)
library(httr)
library(openxlsx)
library(zipangu)

#東北、関東信越、近畿、中国、四国
df_kouseikyoku <- dplyr::tibble(
  url = c(
    "https://kouseikyoku.mhlw.go.jp/tohoku/gyomu/gyomu/hoken_kikan/documents/201805koushin.html",
    "https://kouseikyoku.mhlw.go.jp/kantoshinetsu/chousa/kijyun.html",
    "https://kouseikyoku.mhlw.go.jp/shikoku/gyomu/gyomu/hoken_kikan/shitei/index.html",
    "https://kouseikyoku.mhlw.go.jp/kinki/gyomu/gyomu/hoken_kikan/shitei_jokyo_00004.html",
    "https://kouseikyoku.mhlw.go.jp/chugokushikoku/chousaka/shisetsukijunjuri.html"),
  contains_jp = c("Excel（ZIP）", "医科（ZIP）", "（ZIP）", "（ZIP）", "各県全体（ZIP）")) |> 
    mutate(a_con = str_c("a:contains('", contains_jp, "')"))
  
map(1:nrow(df_kouseikyoku), ~{
# ターゲットURL
url <- df_kouseikyoku$url[.x]
a_contains <- df_kouseikyoku$a_con[.x]
# ウェブページを読み込み、必要なリンクを抽出する
web_page <- read_html(url)
links <- web_page %>% html_nodes(a_contains) %>% html_attr("href")

# ベースURLを定義する
base_url <- "https://kouseikyoku.mhlw.go.jp"

# リンクの完全なURLを生成する
full_links <- paste0(base_url, links)

# 各リンク先ファイルのサイズを取得する
get_file_size <- function(url) {
  response <- HEAD(url)
  as.numeric(headers(response)["content-length"])
}

file_sizes <- sapply(full_links, get_file_size)

# 最大サイズのファイルのリンクを取得する
max_index <- which.max(file_sizes)
max_file_url <- full_links[max_index]

# ダウンロードディレクトリを設定する
download_dir <- "downloads"
if (!dir.exists(download_dir)) {
  dir.create(download_dir)
}

# 最大サイズのファイルをダウンロードする
zip_filename <- basename(max_file_url)
zip_filepath <- file.path(download_dir, zip_filename)
download.file(max_file_url, zip_filepath)

# ダウンロードされたファイルを解凍する
unzip_dir <- tempdir()

# システムコマンドを使って解凍する
zip_command <- paste("unzip", shQuote(zip_filepath), "-d", shQuote(unzip_dir))
system(zip_command)

# 元のディレクトリに保存するためのパスを確認する
original_dir <- dirname(download_dir)

# 解凍されたフォルダ内のエクセルファイルを検索し、元のディレクトリにコピーする
unzip_subdirs <- list.dirs(unzip_dir, recursive = TRUE)

for (subdir in unzip_subdirs) {
  excel_files <- list.files(subdir, pattern = "\\.xlsx$", full.names = TRUE)
  if (length(excel_files) > 0) {
    file.copy(excel_files, original_dir, overwrite = TRUE)
  }
}

#downloadディレクトリを削除する
unlink(download_dir, recursive = TRUE)
})

#北海道
url <- "https://kouseikyoku.mhlw.go.jp/hokkaido/gyomu/gyomu/hoken_kikan/todokede_juri_ichiran.html"

# ウェブページのHTMLを読み込む
webpage <- read_html(url)

# エクセルファイルへのリンクを取得
links <- webpage %>%
  html_nodes("a") %>%
  html_attr("href")

# エクセルファイルのリンクを特定
excel_links <- links[grepl("\\.xlsx$", links)]

# エクセルファイルが存在するか確認
if (length(excel_links) == 0) {
  stop("エクセルファイルのリンクが見つかりませんでした。")
}

# ダウンロードしたファイル名を格納するリスト
downloaded_files <- c()

# ベースURLとファイルリンクを結合し、すべてのエクセルファイルをダウンロード
for (link in excel_links) {
  # 絶対URLに変換
  download_link <- paste0("https://kouseikyoku.mhlw.go.jp", link)
  
  # ファイル名を抽出
  file_name <- basename(link)
  
  # ファイルをダウンロードして保存
  GET(download_link, write_disk(file_name, overwrite = TRUE))
  cat(file_name, "のダウンロードが完了しました。\n")
  
  # ダウンロードしたファイル名をリストに追加
  downloaded_files <- c(downloaded_files, file_name)
}

# ファイルサイズを取得して最大のファイルを特定
file_sizes <- sapply(downloaded_files, file.size)
largest_file <- downloaded_files[which.max(file_sizes)]

cat("最大ファイルは:", largest_file, "です。\n")

# 他のファイルを削除
files_to_delete <- setdiff(downloaded_files, largest_file)
sapply(files_to_delete, file.remove)

#東海
# ダウンロード対象ページのURL
url <- "https://kouseikyoku.mhlw.go.jp/tokaihokuriku/newpage_00349.html"

# ウェブページを読み込み
webpage <- read_html(url)

# 対象リンクを特定
links <- webpage %>%
  html_nodes("a") %>%
  .[str_detect(html_text(.), "^届出受理医療機関名簿（医科）")] %>%
  html_attr("href")

# 最初のリンクを取得
first_link <- links[1]

# 完全なURLを作成
file_url <- paste0("https://kouseikyoku.mhlw.go.jp", first_link)

# ダウンロードディレクトリを設定する
download_dir <- "downloads"
if (!dir.exists(download_dir)) {
  dir.create(download_dir)
}

# 該当ファイルをダウンロードする
zip_filename <- basename(file_url)
zip_filepath <- file.path(download_dir, zip_filename)
download.file(file_url, zip_filepath)

# ダウンロードされたファイルを解凍する
unzip_dir <- tempdir()

# システムコマンドを使って解凍する
zip_command <- paste("unzip", shQuote(zip_filepath), "-d", shQuote(unzip_dir))
system(zip_command)

# 元のディレクトリに保存するためのパスを確認する
original_dir <- dirname(download_dir)

# 解凍されたフォルダ内のエクセルファイルを検索し、元のディレクトリにコピーする
unzip_subdirs <- list.dirs(unzip_dir, recursive = TRUE)

for (subdir in unzip_subdirs) {
  excel_files <- list.files(subdir, pattern = "\\.xls(x)?$", full.names = TRUE)
  if (length(excel_files) > 0) {
    file.copy(excel_files, original_dir, overwrite = TRUE)
  }
}

#downloadディレクトリを削除する
unlink(download_dir, recursive = TRUE)

#九州
# ターゲットURL
url <- "https://kouseikyoku.mhlw.go.jp/kyushu/gyomu/gyomu/hoken_kikan/index_00007.html"

# ウェブページを読み込み、必要なリンクを抽出する
web_page <- read_html(url)
links <- web_page %>% html_nodes("a:contains('エクセルデータ（ZIP）')") %>% html_attr("href")

# 最新ファイル判定のための正規表現を作成
latest_prefix <- max(gsub("^.*/([^_]+_[0-9]+)_.*$", "\\1", links))

# ダウンロードディレクトリを設定する
download_dir <- "downloads"
if (!dir.exists(download_dir)) {
  dir.create(download_dir)
}

# ダウンロードディレクトリを設定する
download_dir <- "downloads"
if (!dir.exists(download_dir)) {
  dir.create(download_dir)
}

# 最新のリンクのみを抽出してダウンロード
latest_links <- links[grepl(paste0(latest_prefix), links)]
  
for (link in latest_links) {
  zip_url <- paste0("https://kouseikyoku.mhlw.go.jp", link)
  zip_filename <- basename(zip_url)
  zip_filepath <- file.path(download_dir, zip_filename)
  download.file(zip_url, zip_filepath)
}

list.files("./downloads")

# 解凍用ディレクトリを設定する
unzip_dir <- "unzipped"
if (!dir.exists(unzip_dir)) {
  dir.create(unzip_dir)
}

# 解凍したファイルをフィルタリングして移動する
target_dir <- "target_files"
if (!dir.exists(target_dir)) {
  dir.create(target_dir)
}

# 元のディレクトリに移動するための作業ディレクトリを取得
original_dir <- getwd()

# ダウンロードしたZIPファイルを解凍し、条件に合うファイルを移動する
zip_files <- list.files(download_dir, pattern = "\\.zip$", full.names = TRUE)
for (zip_file in zip_files) {
  unzip(zip_file, exdir = unzip_dir)
  files <- list.files(unzip_dir,
     pattern = str_c(latest_prefix, ".*_ika_.*\\.xlsx$"), full.names = TRUE)
  if (length(files) > 0) {
    file.copy(files, original_dir, overwrite = TRUE) 
  }
  # 解凍したファイルを削除
  unlink(list.files(unzip_dir, full.names = TRUE, recursive = TRUE))
}

# 使用したディレクトリを削除する
unlink(download_dir, recursive = TRUE)
unlink(unzip_dir, recursive = TRUE)
unlink(target_dir, recursive = TRUE)

#ダウンロードしたファイルを読み込みデータ処理
df_file <- dplyr::tibble(name = list.files()) |> 
  # mutate(path = str_c("./todokede/", name)) |> 
  mutate(extension = str_sub(name, -5, -1)) |> 
  dplyr::filter(extension == ".xlsx")

#47都道府県のエクセルデータを結合。
df_raw <- map(df_file$name, function(fn_xlsx_file) {
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

#各都道府県のエクセルファイルの削除。必要に応じてコメントアウトを。
file.remove(df_file$name)

#ファイル出力(.rdsは約15MB、.csvは約300MB、.xlsxだと約80MBのファイルが出力される。)
#write.xlsx()は若干時間がかかる。
saveRDS(df_raw, str_c("rawfile_", Sys.Date(),".rds"))
# write_excel_csv(df_raw, str_c("rawfile_",Sys.Date(),".csv"))
# write.xlsx(df_raw, str_c("rawfile_",Sys.Date(),".xlsx"))

#都道府県ごとの更新日を.csv出力
write_excel_csv(piv_pref_latest, str_c("update_date_pref_", Sys.Date(), ".csv"))