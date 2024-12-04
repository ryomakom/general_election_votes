# 衆院選の開票結果を整形


<div align="right">
朝日新聞デジタル企画報道部　小宮山亮磨  <br>
@ryomakom  <br>
2024/12/04  </div>

総務省がウェブサイトで[公表](https://www.soumu.go.jp/senkyo/senkyo_s/data/shugiin/ichiran.html)している衆院選の結果（2009年以降の6回分）について、まとめてダウンロードしてクリーニングしてから出力するコードでございます。小選挙区と比例区の二つがあります。

まず必要なパッケージを取得。
```{r library, warning=FALSE,message=FALSE}
library(rvest)
library(httr)
library(readxl)
library(tidyverse)
```


2009～2021年の5回分の選挙データを総務省のサイトからデータをダウンロードし、整形し、合体させる。
```{r download2009_2021, message=FALSE}

# それぞれの選挙の結果を見られるサイトのURLを記録したCSVを読み込んで保存する
urls <- bind_rows(read_csv("data/urls09.csv") %>% mutate(year=2009),
                  read_csv("data/urls12.csv") %>% mutate(year=2012),
                  read_csv("data/urls14.csv") %>% mutate(year=2014),
                  read_csv("data/urls17.csv") %>% mutate(year=2017),
                  read_csv("data/urls21.csv") %>% mutate(year=2021)) %>%
    mutate(url=str_c("https://www.soumu.go.jp",url)) %>% 
    mutate(url_year=str_c(url,year))


# 小選挙区の得票数データを整形するための関数を定義する
update_column_names <- function(tbl) {
    # 1行目の値を取得
    first_row <- slice(tbl, 1)
    
    # 新しい列名を生成
    new_col_names <- colnames(tbl)
    new_col_names[-1] <- paste(new_col_names[-1], first_row[1, -1], sep = "_")
    
    # 列名を更新
    colnames(tbl) <- new_col_names
    
    # 1行目を削除
    tbl <- slice(tbl, -1)
    
    return(tbl)
}

# 最終的なデータフレームの初期化
sing <- tibble() # 小選挙区
prop <- tibble() # 比例区

save_folder <- "data/files09_21" # ダウンロードしたエクセルファイルを保存するフォルダを指定

# 各URLに対して処理を行う
for (url_year in urls$url_year) { # URLと「年」が合体した文字列の一つ一つについて処理をする
    webpage <- read_html(url_year %>% str_sub(-100, -5)) # 文字列に含まれるURLからコンテンツを取得
    which_year <- url_year %>% str_sub(-4, -1) # 文字列から「年」を取得
    excel_links <- html_nodes(webpage, "a") %>% # Excelファイルのリンクを見つける
        html_attr("href") %>%
        .[grepl(".xlsx$|.xls$", .)]
    
    # ExcelファイルにつながるようにベースとなるURLを追加
    excel_links <- paste0("https://www.soumu.go.jp", excel_links)
    
    # 各Excelファイルをダウンロードしてフォルダに保存し、tibbleとして読み込む
    for (link in excel_links) {
        # URLから拡張子を取得
        file_ext <- if(grepl("\\.xlsx$", link)) {
            ".xlsx"
        } else if(grepl("\\.xls$", link)) {
            ".xls"
        } else {
            stop("Link does not point to a valid Excel file.")
        }
        
        # 保存するファイル名を生成
        file_name <- paste0(save_folder, "/", basename(link))
        
        # ファイルをダウンロードして保存
        GET(link, write_disk(file_name, overwrite = TRUE))
        
        # Excelファイルを読み込む
        if(read_excel(file_name)[1,1] %>% str_detect("小選挙区")) { # 小選挙区のファイルだった場合の処理
            # Excelファイルの全シート名を取得
            sheet_names <- excel_sheets(file_name)
            sing_temp <- map_df(sheet_names, ~read_excel(file_name, skip = 3, sheet = .x) %>% 
                                    update_column_names() %>%
                                    rename(city = 候補者名) %>%
                                    mutate_all(as.character) %>%
                                    pivot_longer(cols = c(-city), names_to = "cand", values_to = "vote") %>%
                                    filter(!is.na(city), !str_detect(cand, "NA")) %>%
                                    separate(cand, into = c("cand", "party"), sep = "_") %>%
                                    mutate(dist = .x) %>%
                                    mutate(year = which_year) %>%
                                    select(year,dist,city,party,cand,vote))
            sing <- bind_rows(sing, sing_temp)
        } else {
            # Excelファイルの全シート名を取得
            sheet_names <- excel_sheets(file_name)
            # 各シートからデータを読み込み、df_prop に結合
            prop_temp <- read_excel(file_name, skip=3, sheet=sheet_names[1]) %>%
                mutate_all(as.character) %>%
                rename(city=`市区町村名＼政党名`) %>%
                pivot_longer(cols=c(-city), names_to = "party", values_to = "vote") %>%
                filter(str_sub(party, 1, 3) != "...",!is.na(city)) %>%
                mutate(pref = sheet_names[1]) %>%   # シート名を追加
                mutate(year = which_year) %>% 
                select(year,pref,everything())
            
            prop <- bind_rows(prop, prop_temp)
        }
    }
}
```


2024年の衆院選の結果は、悲しいことにそれ以前とは別形式のウェブページに保存されてる）ので、別のコードで読み込んでいく。

```{r download2024,message=FALSE}

# 2024年の選挙結果がおいてあるURLを指定
url24 <- "https://www.soumu.go.jp/senkyo/senkyo_s/data/shugiin50/shikuchouson.html"

# そのページを読み込む
webpage <- read_html(url24)

# .xlsxファイルのリンクを抽出する
links24 <- webpage %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  .[grepl("\\.xlsx$", .)]

# フルURLを作成する
links24 <- paste0("https://www.soumu.go.jp", links24)

# 総務省が作ったリンクがおかしくて重複がたくさんあるので余計なのを削除（ったくもう…）
links24 <- links24 %>% as_tibble() %>% distinct()
links24 <- links24$value

# 保存先フォルダを指定（相対パス）
save_folder <- "data/files24"

# 各リンク先のエクセルファイルを保存する
for (link in links24) {
  # URLから拡張子を取得
  file_ext <- if (grepl("\\.xlsx$", link)) {
    ".xlsx"
  } else if (grepl("\\.xls$", link)) {
    ".xls"
  } else {
    stop("Link does not point to a valid Excel file.")
  }
  
  # 保存するファイル名を生成
  file_name <- paste0(save_folder, "/", basename(link))
  
  # デバッグ情報を表示
  cat("Downloading file from:", link, "to:", file_name, "\n")
  
  # ファイルをダウンロードして保存
  tryCatch({
    GET(link, write_disk(file_name, overwrite = TRUE))
  }, error = function(e) {
    warning(paste("Failed to download file:", link, "Error:", e$message))
    next
  })
  
  # ファイルが正しくダウンロードされたか確認
  if (!file.exists(file_name)) {
    warning(paste("File does not exist after download:", file_name))
    next
  }
}

# フォルダ内のすべてのエクセルファイルをリストアップ
files24 <- list.files(path = save_folder, pattern = "\\.xlsx$", full.names = TRUE)

# 最終的なデータフレームの初期化
sing24 <- tibble() # 小選挙区
prop24 <- tibble() # 比例区

# 各Excelファイルを読み込み、処理を行う
for (file_name in files24) {
  cat("Reading file:", file_name, "\n")
  
  tryCatch({
    # 小選挙区のファイルだった場合の処理
    if (read_excel(file_name)[1, 1] %>% str_detect("小選挙区")) {
      # Excelファイルの全シート名を取得
      sheet_names <- excel_sheets(file_name)
      sing_temp <- map_df(sheet_names, ~read_excel(file_name, skip = 3, sheet = .x) %>% 
                            update_column_names() %>%
                            rename(city = 候補者名) %>%
                            mutate_all(as.character) %>%
                            pivot_longer(cols = c(-city), names_to = "cand", values_to = "vote") %>%
                            filter(!is.na(city), !str_detect(cand, "NA")) %>%
                            separate(cand, into = c("cand", "party"), sep = "_") %>%
                            mutate(dist = .x) %>%
                            mutate(year = 2024) %>%
                            select(year, dist, city, party, cand, vote))
      sing24 <- bind_rows(sing24, sing_temp)
    } else {
      # 比例代表のファイルだった場合の処理
      sheet_names <- excel_sheets(file_name)
      # "リスト" シートを除外
      sheet_names <- sheet_names[sheet_names != "リスト"]
      prop_temp <- map_df(sheet_names, ~read_excel(file_name, skip = 3, sheet = .x) %>%
                            mutate_all(as.character) %>% 
                            rename(city = `市区町村名＼政党名`) %>% 
                            pivot_longer(cols = c(-city), names_to = "party", values_to = "vote") %>%
                            filter(str_sub(party, 1, 3) != "...", !is.na(city)) %>%
                            mutate(pref = .x) %>% 
                            mutate(year = 2024) %>%
                            select(year, pref, city, party, vote))

      prop24 <- bind_rows(prop24, prop_temp)
    }
  })
}

```


データを合体する

```{r binding, message=FALSE}

sing <- bind_rows(sing %>% mutate(year=as.double(year)),
                  sing24)
prop <- bind_rows(prop %>% mutate(year=as.double(year)),
                  prop24)

```


総務省のデータは入力ミスが無数にあるので、それを一つ一つ修正。空白文字とか余計なものも除去する。

```{r cleaning, message=FALSE}

sing_cleaned <- sing %>%
  mutate(vote=as.double(vote)) %>% 
  mutate(city=str_remove_all(city," "),
         city=str_remove_all(city,"　"),
         party=str_remove_all(party," "),
         party=str_remove_all(party,"　"),
         party=str_remove_all(party," "), # 謎の空白文字？を除去
         cand=str_remove_all(cand," "),
         cand=str_remove_all(cand,"　"),
         cand=str_remove_all(cand,"\n")) %>%
  mutate(party_=ifelse((year==2021 & dist %in% c("福岡県第４区","鹿児島県第３区")),
                       cand,party)) %>% # 政党名と候補者名を逆に入力するミスへの対応
  mutate(cand_=ifelse((year==2021 & dist %in% c("福岡県第４区","鹿児島県第３区")),
                      party,cand)) %>% # 同上
  mutate(party=party_,cand=cand_) %>% 
  select(-party_,-cand_) %>% 
  mutate(vote=ifelse((year==2017 & city=="永平寺町" & cand=="かねもと幸枝"),1228,vote)) %>% # 福井・永平寺町の入力ミス対応
  mutate(vote=ifelse((year==2017 & city=="永平寺町" & cand=="稲田ともみ"),5277,vote)) %>%
  mutate(vote=ifelse((year==2017 & city=="永平寺町" & cand=="鈴木こうじ"),2866,vote)) %>%
  mutate(vote=ifelse((year==2021 & city=="大阪府第19区合計" & cand=="北村みき"),9258,vote)) %>% # 大阪19区の入力ミス対応
  mutate(vote=ifelse((year==2021 & city=="大阪府第19区合計" & cand=="谷川とむ"),52052,vote)) %>%
  mutate(vote=ifelse((year==2021 & city=="大阪府第19区合計" & cand=="いとう信久"),68209,vote)) %>%  
  mutate(vote=ifelse((year==2021 & city=="大阪府第19区合計" & cand=="長安たかし"),32193,vote)) %>%  
  mutate(vote=ifelse((year==2021 & city=="宮城県第３区合計" & cand=="浅田こうじ"),5890,vote)) %>% # 宮城3区の入力ミス対応
  mutate(vote=ifelse((year==2021 & city=="神奈川県第９区合計" & cand=="吉田大成"),24547,vote)) %>% # 神奈川９区の入力もれ対応
  filter(!(year==2021 & cand=="いちじょう芳弘")) %>% # 2017年に宮城３区から出たけど2021年には出てない一条氏のデータを削除
  filter(!(year==2021 & cand=="城内みのる...2")) %>% # 2021年の静岡7区のデータに2017年のデータが残ってるのを削除
  mutate(cand=ifelse(cand=="城内みのる...5","城内みのる",cand)) %>% 
  filter(!(year==2021 & cand=="日吉雄大")) %>% # 同上。しかも「雄太」を「雄大」と誤字
  mutate(cand=ifelse((year==2017 & cand=="日吉雄大"),"日吉雄太",cand)) %>% # 2017年の候補者名誤字を修正
  filter(!(year==2021 & cand=="はたの博")) %>% # 2017年に埼玉14区から出たけど2021年には出てない榛野氏のデータを削除
  mutate(party=ifelse((year==2021 & cand=="すずきのりかず"),"自由民主党",party)) %>% # 2021年の山形２区候補の政党誤入力に対応
  mutate(party=ifelse((year==2021 & cand=="加藤けんいち"),"国民民主党",party)) %>% 
  mutate(party=ifelse((year==2017 & party=="日本共産党" & cand=="石堂あつし"),"自由民主党",party)) %>%
   # 2017年滋賀3区の入力欄まるごと取り違えに対応
  mutate(party=ifelse((year==2017 & party=="自由民主党" & cand=="武村のぶひで"),"日本共産党",party)) %>% 
  mutate(cand=ifelse((year==2017 & party=="日本共産党" & cand=="武村のぶひで"),"石堂あつし",cand)) %>% 
  mutate(cand=ifelse((year==2017 & party=="自由民主党" & cand=="石堂あつし"),"武村のぶひで",cand)) %>% 
  mutate(party=ifelse((year==2009 & cand=="沖ゆり"),"不明",party)) %>% # 2009年広島４区の沖氏の政党誤入力への対応
  mutate(party=ifelse((str_detect(party,"NHK") | str_detect(party,"ＮＨＫ")),"NHK党",party)) %>% # N党の表記ブレ対応
  mutate(party=ifelse(str_detect(party,"自由民主党"),"自由民主党",party)) %>% 
  mutate(party=ifelse(str_detect(party,"立憲民主党"),"立憲民主党",party)) %>% 
  mutate(party=ifelse(str_detect(party,"日本維新"),"日本維新の会",party)) %>%
  mutate(party=ifelse(str_detect(party,"公明"),"公明党",party)) %>%
  mutate(party=ifelse((year==2012 & str_detect(dist,"大阪府") & cand=="中村勝"),
                      "二十一世紀日本維新会",party)) %>% # 日本維新の会に名前が似てるけど別物の政党を排除
  mutate(party=ifelse(str_detect(party,"幸福実現党"),"幸福実現党",party)) %>%
  mutate(party=ifelse(str_detect(party,"社民党"),"社会民主党",party)) %>%
  mutate(party=ifelse(str_detect(party,"無所属"),"無所属",party)) %>%
  mutate(party=str_remove_all(party,"\\(|\\)")) %>%
  mutate(party=str_remove_all(party,"\\[|\\]")) %>%
  mutate(party=str_remove_all(party,"（")) %>% 
  mutate(party=str_remove_all(party,"）")) %>%   
  mutate(dist=stringi::stri_trans_general(dist, "Fullwidth-Halfwidth")) %>% # 選挙区名全角数字を半角に
  mutate(city=stringi::stri_trans_general(city, "Fullwidth-Halfwidth")) %>%
  mutate(dist=str_remove(dist,"第")) # 選挙区名の「第」を省略

prop_cleaned <- prop %>%
  mutate(city=str_remove_all(city," "),
         city=str_remove_all(city,"　"),
         city=str_remove_all(city,"\n"),
         party=str_remove_all(party," "),
         party=str_remove_all(party,"　"),
         party=str_remove_all(party," "), # 謎の空白文字？を除去
         party=str_remove_all(party,"\n"),
         party=str_remove_all(party,"\r")) %>% 
  mutate(party=ifelse((str_detect(party,"NHK") |
                         str_detect(party,"ＮＨＫ")),
                      "NHK党",party)) %>% # N党の表記ブレ対応
  mutate(vote=ifelse((year==2009 &
                        pref=="滋賀県" &
                        city=="安土町" &
                        party=="得票数計"),
                     6850,vote)) %>% # 記入漏れ対応
  mutate(vote=as.double(vote)) %>% 
  filter(!is.na(vote)) %>%
  # 仙台市太白区、静岡市葵区と清水区、愛知県瀬戸市などでほかの市や区と合同で計上しているという注釈を削除
  mutate(city=ifelse((pref=="大分県" & city=="佐伯氏"),"佐伯市",city)) %>% # 大分県佐伯市の表記ミス
  mutate(city=ifelse(city=="栗国村","粟国村",city)) # 沖縄県粟国村の表記ミス

prop_cleaned <- prop_cleaned %>% 
    filter(party!="得票数計") %>% # 各開票区における得票数計のデータが誤っているものが多数あったので、自力で計算
    bind_rows(prop_cleaned %>%
                  filter(party!="得票数計") %>%
                  group_by(year,pref,city) %>%
                  summarize(vote=sum(vote)) %>%
                  mutate(party="得票数計"))
```

クリーニングしたデータをCSVとして保存する。

```{r output, message=FALSE}

sing_cleaned %>% write_excel_csv("data/小選挙区.csv")
prop_cleaned %>% write_excel_csv("data/比例区.csv")

```

何ができるかテストするため、比例区のデータを使って、公明党の得票率のシェアが各都道府県でどう推移したかを示す地図を作ってみる。
```{r test}

library(sf)
jp_pref <- st_read("data/jp_pref.geojson")
komei_ratio <- prop_cleaned %>%
  filter(str_detect(city,"合計")) %>%
  pivot_wider(names_from = party,values_from = vote) %>%
  mutate(rkomei=公明党/得票数計) %>%
  select(year,pref,rkomei) %>%
  mutate(year=str_c("y",year)) %>%
  pivot_wider(names_from = year,values_from = rkomei)

jp_pref %>%
  left_join(komei_ratio %>%
              rename(name=pref)) %>%
  ggplot() +
  geom_sf(aes(geometry=geometry,fill=y2009)) +
  scale_fill_gradient2(high="red",low="blue",mid = "white",
                       midpoint = 0.12,
                       limits=c(0,0.2)) +
  theme_void() +
  coord_sf(xlim = c(127, 147), ylim = c(25, 46),
           crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"), 
           expand = FALSE) +
  labs(title="2009年衆院選の公明党得票シェア")

```
![](img/2009.jpg)


```{r test}

jp_pref %>%
  left_join(komei_ratio %>%
              rename(name=pref)) %>%
  ggplot() +
  geom_sf(aes(geometry=geometry,fill=y2012)) +
  scale_fill_gradient2(high="red",low="blue",mid = "white",
                       midpoint = 0.12,
                       limits=c(0,0.2)) +
  theme_void() +
  coord_sf(xlim = c(127, 147), ylim = c(25, 46),
           crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"), 
           expand = FALSE) +
  labs(title="2012年衆院選の公明党得票シェア")
```

![](img/2012.jpg)

```{r test}

jp_pref %>%
  left_join(komei_ratio %>%
              rename(name=pref)) %>%
  ggplot() +
  geom_sf(aes(geometry=geometry,fill=y2014)) +
  scale_fill_gradient2(high="red",low="blue",mid = "white",
                       midpoint = 0.12,
                       limits=c(0,0.2)) +
  theme_void() +
  coord_sf(xlim = c(127, 147), ylim = c(25, 46),
           crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"), 
           expand = FALSE) +
  labs(title="2014年衆院選の公明党得票シェア")
```
![](img/2014.jpg)


```{r test}

jp_pref %>%
  left_join(komei_ratio %>%
              rename(name=pref)) %>%
  ggplot() +
  geom_sf(aes(geometry=geometry,fill=y2017)) +
  scale_fill_gradient2(high="red",low="blue",mid = "white",
                       midpoint = 0.12,
                       limits=c(0,0.2)) +
  theme_void() +
  coord_sf(xlim = c(127, 147), ylim = c(25, 46),
           crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"), 
           expand = FALSE) +
  labs(title="2017年衆院選の公明党得票シェア")
```

![](img/2017.jpg)

```{r test}

jp_pref %>%
  left_join(komei_ratio %>%
              rename(name=pref)) %>%
  ggplot() +
  geom_sf(aes(geometry=geometry,fill=y2021)) +
  scale_fill_gradient2(high="red",low="blue",mid = "white",
                       midpoint = 0.12,
                       limits=c(0,0.2)) +
  theme_void() +
  coord_sf(xlim = c(127, 147), ylim = c(25, 46),
           crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"), 
           expand = FALSE) +
  labs(title="2021年衆院選の公明党得票シェア")
```
![](img/2021.jpg)


```{r test}

jp_pref %>%
  left_join(komei_ratio %>%
              rename(name=pref)) %>%
  ggplot() +
  geom_sf(aes(geometry=geometry,fill=y2024)) +
  scale_fill_gradient2(high="red",low="blue",mid = "white",
                       midpoint = 0.12,
                       limits=c(0,0.2)) +
  theme_void() +
  coord_sf(xlim = c(127, 147), ylim = c(25, 46),
           crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"), 
           expand = FALSE) +
  labs(title="2024年衆院選の公明党得票シェア")

```
![](img/2024.jpg)

以上。
