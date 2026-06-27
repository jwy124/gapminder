# EDA.R — Gapminder 탐색적 데이터 분석(Exploratory Data Analysis)
# 대상: data/gapminder.csv
# 실행: Rscript EDA.R
# 산출: 콘솔 요약 통계 + figures/ 폴더에 PNG 차트 8종
# 의존: ggplot2, dplyr, readr (base graphics 미사용)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
})

options(width = 100)

# ---------------------------------------------------------------------------
# 0. 로드 & 준비
# ---------------------------------------------------------------------------
infile <- file.path("data", "gapminder.csv")
stopifnot(file.exists(infile))
gap <- read_csv(infile, show_col_types = FALSE)

figdir <- "figures"
if (!dir.exists(figdir)) dir.create(figdir)
save_plot <- function(p, name, w = 9, h = 5.5) {
  path <- file.path(figdir, name)
  ggsave(path, p, width = w, height = h, dpi = 110)
  cat("  [그림 저장]", path, "\n")
}

# 공통 테마/팔레트
theme_set(theme_minimal(base_size = 12))
cont_colors <- c(Africa = "#E69F00", Americas = "#56B4E9", Asia = "#009E73",
                 Europe = "#CC79A7", Oceania = "#0072B2")

hr <- function(t) cat("\n", strrep("=", 76), "\n## ", t, "\n", strrep("=", 76), "\n", sep = "")

# ---------------------------------------------------------------------------
# 1. 데이터 개요
# ---------------------------------------------------------------------------
hr("1. 데이터 개요")
cat(sprintf("관측치: %d행 × %d열\n", nrow(gap), ncol(gap)))
cat(sprintf("국가: %d개 | 대륙: %d개 | 연도: %d~%d (%d개 시점)\n",
            n_distinct(gap$country), n_distinct(gap$continent),
            min(gap$year), max(gap$year), n_distinct(gap$year)))
cat("\n수치형 변수 요약:\n")
print(summary(gap[c("year", "lifeExp", "pop", "gdpPercap")]))

# ---------------------------------------------------------------------------
# 2. 단변량 분포(Univariate)
# ---------------------------------------------------------------------------
hr("2. 단변량 분포")

# 왜도(skewness) 간단 계산
skew <- function(x) { x <- x[!is.na(x)]; m <- mean(x); mean((x-m)^3)/ (mean((x-m)^2))^1.5 }
for (v in c("lifeExp", "gdpPercap", "pop")) {
  cat(sprintf("  %-10s 왜도(skewness) = %6.2f %s\n", v, skew(gap[[v]]),
              ifelse(abs(skew(gap[[v]])) > 1, "(강한 우편향 → 로그 변환 권장)", "")))
}

# 그림 1: 기대수명 히스토그램
p1 <- ggplot(gap, aes(lifeExp)) +
  geom_histogram(bins = 30, fill = "#56B4E9", color = "white") +
  labs(title = "기대수명 분포 (전체 연도)", x = "기대수명(세)", y = "관측치 수")
save_plot(p1, "01_hist_lifeExp.png")

# 그림 2: GDP 분포 — 원자료 vs 로그
p2 <- ggplot(gap, aes(gdpPercap)) +
  geom_histogram(bins = 40, fill = "#009E73", color = "white") +
  scale_x_log10(labels = scales::comma) +
  labs(title = "1인당 GDP 분포 (로그 스케일)",
       subtitle = "원자료는 강한 우편향 → 로그 변환 시 종형에 가까워짐",
       x = "1인당 GDP (국제달러, log10)", y = "관측치 수")
save_plot(p2, "02_hist_gdp_log.png")

# ---------------------------------------------------------------------------
# 3. 대륙별 분포(2007) — 박스플롯
# ---------------------------------------------------------------------------
hr("3. 대륙별 분포 (2007)")
g07 <- filter(gap, year == 2007)
cat("기대수명 요약통계(2007):\n")
print(g07 %>% group_by(continent) %>%
        summarise(n = n(), 평균 = mean(lifeExp), 중앙값 = median(lifeExp),
                  표준편차 = sd(lifeExp), .groups = "drop") %>%
        mutate(across(where(is.numeric), ~round(., 2))))

p3 <- ggplot(g07, aes(reorder(continent, lifeExp, median), lifeExp, fill = continent)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.5) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1) +
  scale_fill_manual(values = cont_colors, guide = "none") +
  coord_flip() +
  labs(title = "대륙별 기대수명 분포 (2007)", x = NULL, y = "기대수명(세)")
save_plot(p3, "03_box_lifeExp_2007.png")

# ---------------------------------------------------------------------------
# 4. 시계열 추세 — 대륙 평균
# ---------------------------------------------------------------------------
hr("4. 시계열 추세 (대륙 평균)")
trend <- gap %>% group_by(continent, year) %>%
  summarise(lifeExp = mean(lifeExp), gdpPercap = mean(gdpPercap), .groups = "drop")

p4 <- ggplot(trend, aes(year, lifeExp, color = continent)) +
  geom_line(linewidth = 1.1) + geom_point(size = 1.6) +
  scale_color_manual(values = cont_colors) +
  labs(title = "대륙별 평균 기대수명 추이 (1952–2007)",
       x = "연도", y = "기대수명(세)", color = "대륙")
save_plot(p4, "04_trend_lifeExp.png")

p5 <- ggplot(trend, aes(year, gdpPercap, color = continent)) +
  geom_line(linewidth = 1.1) + geom_point(size = 1.6) +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = cont_colors) +
  labs(title = "대륙별 평균 1인당 GDP 추이 (로그 스케일)",
       x = "연도", y = "1인당 GDP (log10)", color = "대륙")
save_plot(p5, "05_trend_gdp.png")

# ---------------------------------------------------------------------------
# 5. 이변량 관계 — 기대수명 vs GDP
# ---------------------------------------------------------------------------
hr("5. 기대수명 vs 1인당 GDP")
cat(sprintf("전체 상관계수  raw r = %.3f | log(GDP) r = %.3f\n",
            cor(gap$lifeExp, gap$gdpPercap),
            cor(gap$lifeExp, log(gap$gdpPercap))))
cat("\n대륙별 상관(log GDP):\n")
print(gap %>% group_by(continent) %>%
        summarise(r_log = round(cor(lifeExp, log(gdpPercap)), 3), .groups = "drop"))

# 그림 6: 산점도 2007 (버블 = 인구)
p6 <- ggplot(g07, aes(gdpPercap, lifeExp, color = continent, size = pop)) +
  geom_point(alpha = 0.7) +
  scale_x_log10(labels = scales::comma) +
  scale_size(range = c(1.5, 16), guide = "none") +
  scale_color_manual(values = cont_colors) +
  labs(title = "기대수명 vs 1인당 GDP (2007, 버블=인구)",
       subtitle = "로그 소득에 대해 기대수명은 거의 선형 → 수확체감 관계",
       x = "1인당 GDP (국제달러, log10)", y = "기대수명(세)", color = "대륙")
save_plot(p6, "06_scatter_2007.png")

# 그림 7: 전체 연도 산점 + 추세선
p7 <- ggplot(gap, aes(gdpPercap, lifeExp)) +
  geom_point(aes(color = continent), alpha = 0.25, size = 1) +
  geom_smooth(method = "loess", formula = y ~ x, color = "black", se = FALSE) +
  scale_x_log10(labels = scales::comma) +
  scale_color_manual(values = cont_colors) +
  labs(title = "기대수명 vs 1인당 GDP (전체 연도 + LOESS 추세선)",
       x = "1인당 GDP (log10)", y = "기대수명(세)", color = "대륙")
save_plot(p7, "07_scatter_all_loess.png")

# ---------------------------------------------------------------------------
# 6. 국가 궤적 — 대륙별 facet
# ---------------------------------------------------------------------------
hr("6. 국가별 궤적 (대륙 facet)")
p8 <- ggplot(gap, aes(year, lifeExp, group = country, color = continent)) +
  geom_line(alpha = 0.4) +
  facet_wrap(~continent, nrow = 1) +
  scale_color_manual(values = cont_colors, guide = "none") +
  labs(title = "국가별 기대수명 궤적 (대륙별)",
       subtitle = "아프리카의 1990년대 급락(에이즈·분쟁)이 개별 선으로 드러남",
       x = "연도", y = "기대수명(세)")
save_plot(p8, "08_facet_country_traj.png", w = 12, h = 4.5)

# ---------------------------------------------------------------------------
# 7. 주요 발견 요약
# ---------------------------------------------------------------------------
hr("주요 발견(Key findings)")
cat("  1) gdpPercap·pop은 강한 우편향 → 로그 변환/중앙값 비교 권장\n")
cat("  2) 기대수명-소득은 비선형(로그형). log(GDP)에서 상관이 0.58→0.81로 강화\n")
cat("  3) 대륙 간 수명 격차는 수렴, 소득 격차는 발산\n")
cat("  4) 아프리카는 1990년대 정체/후퇴 — facet 궤적에서 개별 급락선 확인\n")
cat(sprintf("\n  → figures/ 폴더에 차트 8종 저장 완료. (%s)\n",
            format(Sys.time(), "%Y-%m-%d %H:%M")))
