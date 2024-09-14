library(conflicted)
library(tidyverse)
library(stringi)
library(rvest)
library(httr)

# URLを指定
url <- c("https://kouseikyoku.mhlw.go.jp/kantoshinetsu/shinsei/shido_kansa/shitei_kijun/kihon_shinryo_r06.html",
"https://kouseikyoku.mhlw.go.jp/kantoshinetsu/shinsei/shido_kansa/shitei_kijun/tokukei_shinryo_r06.html")

df_con <- map(url, ~{
  # SSLエラーを回避するための設定
  set_config(config(ssl_verifypeer = 0L))

  # ウェブページを読み込む
  webpage <- read_html(.x)

  # classがdatatableのすべてのテーブルを取得
  tables <- webpage %>% 
    html_nodes("table.datatable") %>%
    html_table()

  # 2番目のテーブルを選択
  tables[[2]] |> 
    select(1, 2) |> 
    rename(num = 1, item = 2) |> 
    slice(-1, -2)
}) |> list_rbind() |> 
  mutate(item = stri_trans_nfkc(item)) |> 
  mutate(item = gsub("[0-9]+$", "", item)) |> 
  distinct(item, .keep_all = TRUE) |> 
  mutate(num_work = sub(".*-", "", num)) |> 
  mutate(num_work = str_pad(num_work, 3, side = "left", pad = "0")) |> 
  mutate(num = str_c(str_sub(num, 1, 2), num_work)) |> 
  select(-num_work)