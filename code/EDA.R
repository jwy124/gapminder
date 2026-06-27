# EDA.R — Gapminder 탐색적 데이터 분석 (개정판)
# 대상: data/gapminder.csv
# 실행: 프로젝트 루트에서  Rscript code/EDA.R
# 산출: 콘솔 요약 통계 + figures/ 폴더 PNG 차트 + figures/session_info.txt
# 의존: ggplot2, dplyr, readr (base graphics 미사용)
#
# [개정 동기] 초판 EDA의 한계를 비판적으로 보완:
#   1) 비가중 평균만 사용 → 인구가중("평균적인 사람") 관점 추가
#   2) 수렴/발산을 주장만 함 → σ-수렴·β-수렴으로 정량화
#   3) 풀링된 이봉형 착시 → 연도별 분포 이동으로 정정
#   4) 이상치를 세기만 함 → 이름을 붙이고 소득 대비 잔차로 식별
#   5) 상관계수만 제시 → 선형모형 R²·연도별 상관추이 추가
#   6) Oceania(n=2) 동급 취급 → 명시적 캐비엇/분리
#   7) 재현성 부재 → set.seed + sessionInfo 기록

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(ggplot2)
})
options(width = 100)
set.seed(42)  # jitter 등 난수 재현성

# ---------------------------------------------------------------------------
# 0. 로드 & 공통 유틸
# ---------------------------------------------------------------------------
# 루트/‘code’ 어디서 실행해도 data/·figures/가 루트 기준이 되도록 보정
if (!file.exists("data/gapminder.csv") && file.exists("../data/gapminder.csv")) setwd("..")

infile <- file.path("data", "gapminder.csv")
stopifnot(file.exists(infile))
gap <- read_csv(infile, show_col_types = FALSE)
gap$loggdp <- log(gap$gdpPercap)

figdir <- "figures"
if (!dir.exists(figdir)) dir.create(figdir)
save_plot <- function(p, name, w = 9, h = 5.5) {
  path <- file.path(figdir, name)
  ggsave(path, p, width = w, height = h, dpi = 110)
  cat("  [그림]", path, "\n")
}
theme_set(theme_minimal(base_size = 12))
cont_colors <- c(Africa = "#E69F00", Americas = "#56B4E9", Asia = "#009E73",
                 Europe = "#CC79A7", Oceania = "#0072B2")
hr <- function(t) cat("\n", strrep("=", 76), "\n## ", t, "\n", strrep("=", 76), "\n", sep = "")
skew <- function(x){x<-x[!is.na(x)];m<-mean(x);mean((x-m)^3)/(mean((x-m)^2))^1.5}
kurt <- function(x){x<-x[!is.na(x)];m<-mean(x);mean((x-m)^4)/(mean((x-m)^2))^2-3}

FIRST <- min(gap$year); LAST <- max(gap$year)

# ---------------------------------------------------------------------------
# 1. 개요 + 구조적 주의사항
# ---------------------------------------------------------------------------
hr("1. 데이터 개요")
cat(sprintf("관측치 %d행 × %d열 | 국가 %d | 대륙 %d | 연도 %d~%d (%d시점)\n",
            nrow(gap), ncol(gap)-1, n_distinct(gap$country),
            n_distinct(gap$continent), FIRST, LAST, n_distinct(gap$year)))
cat("\n대륙별 국가 수 (※ Oceania=2 → 대륙 내 분산통계는 해석 주의):\n")
print(gap %>% distinct(country, continent) %>% count(continent, name = "n_countries"))
cat("\n수치형 요약:\n"); print(summary(gap[c("year","lifeExp","pop","gdpPercap")]))

# ---------------------------------------------------------------------------
# 2. 단변량 분포 + 형태 진단 + 극단값 명명
# ---------------------------------------------------------------------------
hr("2. 단변량 분포 (왜도·첨도 + 극단값)")
shape <- tibble(
  변수 = c("lifeExp","gdpPercap","pop"),
  왜도 = c(skew(gap$lifeExp), skew(gap$gdpPercap), skew(gap$pop)),
  첨도 = c(kurt(gap$lifeExp), kurt(gap$gdpPercap), kurt(gap$pop))
) %>% mutate(across(where(is.numeric), ~round(.,2)),
             진단 = ifelse(abs(왜도) > 1, "강한 우편향 → 로그/중앙값 권장", "근사 대칭"))
print(shape)

cat("\n극단값(전 기간 기준):\n")
cat("  최저 기대수명:", with(gap[which.min(gap$lifeExp),],
      sprintf("%s %d (%.1f세)", country, year, lifeExp)), "\n")
cat("  최고 1인당GDP:", with(gap[which.max(gap$gdpPercap),],
      sprintf("%s %d ($%s)", country, year, format(round(gdpPercap),big.mark=","))), "\n")

p1 <- ggplot(gap, aes(lifeExp)) +
  geom_histogram(bins = 30, fill = "#56B4E9", color = "white") +
  labs(title = "기대수명 분포 (전 연도 풀링)", x = "기대수명(세)", y = "관측치")
save_plot(p1, "01_hist_lifeExp.png")

p2 <- ggplot(gap, aes(gdpPercap)) +
  geom_histogram(bins = 40, fill = "#009E73", color = "white") +
  scale_x_log10(labels = scales::comma) +
  labs(title = "1인당 GDP 분포 (로그 스케일)",
       subtitle = sprintf("원자료 왜도 %.1f → 로그 변환 시 종형 근사", skew(gap$gdpPercap)),
       x = "1인당 GDP (log10)", y = "관측치")
save_plot(p2, "02_hist_gdp_log.png")

# ---------------------------------------------------------------------------
# 3. [보완] 분포의 시간적 이동 — 풀링 이봉형의 정체
# ---------------------------------------------------------------------------
hr("3. 분포의 시간적 이동 (풀링 이봉형 ≠ 단일연도)")
cat("초판의 '이봉형' 해석은 전 연도 풀링이 만든 착시. 연도별로 보면\n",
    "저수명 봉우리가 우측(고수명)으로 이동하며 단봉형에 수렴한다.\n", sep="")
yrs <- c(1952, 1977, 2007)
p3 <- ggplot(filter(gap, year %in% yrs),
             aes(lifeExp, fill = factor(year), color = factor(year))) +
  geom_density(alpha = 0.35) +
  scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1") +
  labs(title = "기대수명 분포의 시간 이동",
       subtitle = "1952(이봉) → 1977 → 2007(단봉, 우측 이동)",
       x = "기대수명(세)", y = "밀도", fill = "연도", color = "연도")
save_plot(p3, "03_density_lifeExp_byyear.png")

# ---------------------------------------------------------------------------
# 4. 대륙별 분포 (2007) — Oceania 캐비엇 명시
# ---------------------------------------------------------------------------
hr("4. 대륙별 분포 (2007)")
g07 <- filter(gap, year == LAST)
tab4 <- g07 %>% group_by(continent) %>%
  summarise(n=n(), 평균=mean(lifeExp), 중앙값=median(lifeExp),
            표준편차=sd(lifeExp), IQR=IQR(lifeExp), .groups="drop") %>%
  mutate(across(where(is.numeric), ~round(.,2)))
print(tab4)
cat("※ Oceania(n=2)의 표준편차/IQR은 두 점(호주·뉴질랜드)만의 값 → 비교 부적절.\n")

p4 <- ggplot(g07, aes(reorder(continent, lifeExp, median), lifeExp, fill = continent)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.5) +
  geom_jitter(width = 0.15, alpha = 0.35, size = 1) +
  scale_fill_manual(values = cont_colors, guide = "none") +
  coord_flip() +
  labs(title = "대륙별 기대수명 분포 (2007)",
       subtitle = "Oceania는 표본 2개 — 박스 해석 주의",
       x = NULL, y = "기대수명(세)")
save_plot(p4, "04_box_lifeExp_2007.png")

# ---------------------------------------------------------------------------
# 5. [핵심 보완] 비가중 vs 인구가중 — '평균적 국가' vs '평균적 사람'
# ---------------------------------------------------------------------------
hr("5. 비가중 vs 인구가중 전지구 추세")
glob <- gap %>% group_by(year) %>%
  summarise(le_uw = mean(lifeExp), le_w = weighted.mean(lifeExp, pop),
            gdp_uw = mean(gdpPercap), gdp_w = weighted.mean(gdpPercap, pop),
            .groups = "drop")
print(glob %>% mutate(across(-year, ~round(.,1))))
{
  uw <- glob$le_uw[glob$year==LAST]; w <- glob$le_w[glob$year==LAST]
  uw0 <- glob$le_uw[1]; w0 <- glob$le_w[1]
  cat(sprintf("\n2007 기대수명: 비가중 %.1f vs 인구가중 %.1f (차 %+.1f세)\n", uw, w, w-uw))
  cat(sprintf("1952 기대수명: 비가중 %.1f vs 인구가중 %.1f (차 %+.1f세)\n", uw0, w0, w0-uw0))
  cat("→ 인구가중>비가중이면 인구 대국(중국·인도)의 수명이 평균 이상.\n")
  cat(sprintf("→ 부호 역전(%+.1f→%+.1f): 초기엔 인구 대국이 평균 이하였으나 2007년엔 평균 이상으로 추월.\n",
              w0-uw0, w-uw))
}

glob_long <- bind_rows(
  transmute(glob, year, value = le_uw, 기준 = "비가중(국가 평균)"),
  transmute(glob, year, value = le_w,  기준 = "인구가중(사람 평균)"))
p5 <- ggplot(glob_long, aes(year, value, color = 기준)) +
  geom_line(linewidth = 1.2) + geom_point(size = 1.8) +
  scale_color_manual(values = c("비가중(국가 평균)"="#999999","인구가중(사람 평균)"="#D55E00")) +
  labs(title = "전지구 기대수명: 비가중 vs 인구가중",
       subtitle = "두 선의 간극 = 인구 대국과 소국의 수명 격차",
       x = "연도", y = "기대수명(세)", color = NULL)
save_plot(p5, "05_weighted_vs_unweighted.png")

# 대륙 GDP 추세(로그)는 유지
trend <- gap %>% group_by(continent, year) %>%
  summarise(gdpPercap = mean(gdpPercap), .groups = "drop")
p6 <- ggplot(trend, aes(year, gdpPercap, color = continent)) +
  geom_line(linewidth = 1.1) + geom_point(size = 1.6) +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = cont_colors) +
  labs(title = "대륙별 평균 1인당 GDP 추이 (로그)",
       x = "연도", y = "1인당 GDP (log10)", color = "대륙")
save_plot(p6, "06_trend_gdp.png")

# ---------------------------------------------------------------------------
# 6. [보완] 수렴의 정량화 — σ-수렴 & β-수렴
# ---------------------------------------------------------------------------
hr("6. 수렴 분석 (σ-수렴 & β-수렴)")

# σ-수렴: 연도별 국가 간 분산(변동계수)이 줄어드는가?
sigma <- gap %>% group_by(year) %>%
  summarise(le_cv  = sd(lifeExp)/mean(lifeExp),
            gdp_cv = sd(loggdp)/mean(loggdp), .groups = "drop")
cat("변동계수(CV) 추이 — 값이 줄면 수렴:\n")
print(sigma %>% mutate(across(-year, ~round(.,4))))
cat(sprintf("\n기대수명 CV: %.3f(1952) → %.3f(2007) %s\n",
            sigma$le_cv[1], sigma$le_cv[nrow(sigma)],
            ifelse(sigma$le_cv[nrow(sigma)]<sigma$le_cv[1],"→ 수렴(σ)","→ 발산")))
cat(sprintf("log(GDP) CV: %.3f(1952) → %.3f(2007) %s\n",
            sigma$gdp_cv[1], sigma$gdp_cv[nrow(sigma)],
            ifelse(sigma$gdp_cv[nrow(sigma)]<sigma$gdp_cv[1],"→ 수렴","→ 발산")))

sig_long <- bind_rows(
  transmute(sigma, year, cv = le_cv,  지표 = "기대수명"),
  transmute(sigma, year, cv = gdp_cv, 지표 = "log(1인당 GDP)"))
p7 <- ggplot(sig_long, aes(year, cv, color = 지표)) +
  geom_line(linewidth = 1.2) + geom_point(size = 1.8) +
  labs(title = "σ-수렴: 국가 간 분산(변동계수) 추이",
       subtitle = "기대수명은 수렴(하락), 소득 분산은 상대적으로 완만",
       x = "연도", y = "변동계수 (sd/mean)", color = NULL)
save_plot(p7, "07_sigma_convergence.png")

# β-수렴: 초기(1952) 수준이 낮을수록 이후 성장폭이 큰가?
beta <- inner_join(
  filter(gap, year==FIRST) %>% select(country, continent, le0 = lifeExp),
  filter(gap, year==LAST)  %>% select(country, le1 = lifeExp), by = "country") %>%
  mutate(growth = le1 - le0)
bfit <- lm(growth ~ le0, data = beta)
cat(sprintf("\nβ-수렴 회귀: growth = %.1f + (%.3f)·le0,  기울기 p=%.2e\n",
            coef(bfit)[1], coef(bfit)[2], summary(bfit)$coefficients[2,4]))
cat(ifelse(coef(bfit)[2] < 0,
           "→ 기울기 음수: 초기 저수명 국가가 더 빠르게 성장(β-수렴 성립).\n",
           "→ 기울기 양수: 발산.\n"))
p8 <- ggplot(beta, aes(le0, growth, color = continent)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_smooth(method = "lm", formula = y~x, color = "black", se = TRUE) +
  scale_color_manual(values = cont_colors) +
  labs(title = "β-수렴: 초기 기대수명(1952) vs 이후 증가폭(→2007)",
       subtitle = "음의 기울기 = 가난(단명)했던 나라가 더 크게 따라잡음",
       x = "1952 기대수명(세)", y = "1952→2007 증가폭(세)", color = "대륙")
save_plot(p8, "08_beta_convergence.png")

# ---------------------------------------------------------------------------
# 7. 기대수명 vs 소득 — 모형·연도별 상관·잔차(이상국가)
# ---------------------------------------------------------------------------
hr("7. 기대수명 vs 1인당 GDP (모형 + 잔차)")
fit_all <- lm(lifeExp ~ loggdp, data = gap)
cat(sprintf("전체 선형모형 lifeExp ~ log(GDP):  R² = %.3f  (기울기 %.2f세 / log단위)\n",
            summary(fit_all)$r.squared, coef(fit_all)[2]))

# 연도별 상관: 관계가 강해지는가/약해지는가?
cory <- gap %>% group_by(year) %>%
  summarise(r = cor(lifeExp, loggdp), .groups = "drop")
cat("\n연도별 상관 cor(lifeExp, log GDP):\n"); print(cory %>% mutate(r=round(r,3)))
p9 <- ggplot(cory, aes(year, r)) +
  geom_line(linewidth = 1.2, color = "#0072B2") + geom_point(size = 2) +
  ylim(0, 1) +
  labs(title = "연도별 기대수명–소득 상관계수",
       subtitle = "관계 강도가 시간에 따라 어떻게 변하는가",
       x = "연도", y = "Pearson r (log GDP)")
save_plot(p9, "09_corr_by_year.png")

# 2007 버블 산점
p10 <- ggplot(g07, aes(gdpPercap, lifeExp, color = continent, size = pop)) +
  geom_point(alpha = 0.7) +
  scale_x_log10(labels = scales::comma) +
  scale_size(range = c(1.5, 16), guide = "none") +
  scale_color_manual(values = cont_colors) +
  labs(title = "기대수명 vs 1인당 GDP (2007, 버블=인구)",
       subtitle = "로그 소득에 대해 수명은 거의 선형(수확체감)",
       x = "1인당 GDP (log10)", y = "기대수명(세)", color = "대륙")
save_plot(p10, "10_scatter_2007.png")

# [보완] 잔차: 소득 대비 수명 초과/미달 국가 (Gapminder 핵심 인사이트)
g07r <- g07 %>% mutate(pred = predict(fit_all, .), resid = lifeExp - pred)
over  <- g07r %>% slice_max(resid, n = 5) %>% select(country, continent, gdpPercap, lifeExp, resid)
under <- g07r %>% slice_min(resid, n = 5) %>% select(country, continent, gdpPercap, lifeExp, resid)
cat("\n[소득 대비 장수] 예측보다 오래 사는 5개국 (양의 잔차):\n")
print(over %>% mutate(resid=round(resid,1)))
cat("\n[소득 대비 단명] 예측보다 일찍 죽는 5개국 (음의 잔차, 예: 석유경제·에이즈):\n")
print(under %>% mutate(resid=round(resid,1)))

lab <- bind_rows(over, under)
p11 <- ggplot(g07r, aes(gdpPercap, lifeExp)) +
  geom_point(aes(color = continent), alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", formula = y~log(x), color = "black", se = FALSE) +
  geom_text(data = lab, aes(label = country), size = 3, vjust = -0.8, color = "black") +
  geom_point(data = lab, color = "black", shape = 21, size = 3) +
  scale_x_log10(labels = scales::comma) +
  scale_color_manual(values = cont_colors) +
  labs(title = "소득 대비 기대수명 — 회귀선에서 가장 벗어난 국가",
       subtitle = "위쪽=소득 대비 장수 / 아래쪽=부유하지만 단명(석유경제 등)",
       x = "1인당 GDP (log10)", y = "기대수명(세)", color = "대륙")
save_plot(p11, "11_residual_outliers.png")

# ---------------------------------------------------------------------------
# 8. [보완] 성장의 동조성 — 소득 성장 vs 수명 성장 (국가 단위)
# ---------------------------------------------------------------------------
hr("8. 국가별 성장 동조성 (1952→2007)")
gr <- inner_join(
  filter(gap, year==FIRST) %>% select(country, continent, le0=lifeExp, g0=gdpPercap),
  filter(gap, year==LAST)  %>% select(country, le1=lifeExp, g1=gdpPercap), by="country") %>%
  mutate(le_gain = le1 - le0, gdp_ratio = g1 / g0)
cat(sprintf("cor(log 소득배수, 수명증가폭) = %.3f\n",
            cor(log(gr$gdp_ratio), gr$le_gain)))
cat("최대 수명 증가:", with(gr[which.max(gr$le_gain),], sprintf("%s (+%.1f세)",country,le_gain)),
    "| 수명 후퇴:", with(gr[which.min(gr$le_gain),], sprintf("%s (%.1f세)",country,le_gain)), "\n")
p12 <- ggplot(gr, aes(gdp_ratio, le_gain, color = continent)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(alpha = 0.8, size = 2) +
  scale_x_log10() +
  scale_color_manual(values = cont_colors) +
  labs(title = "소득 성장 vs 수명 성장 (국가별, 1952→2007)",
       subtitle = "음수 영역 = 55년간 기대수명이 후퇴한 국가",
       x = "1인당 GDP 배수 (log10)", y = "기대수명 증가폭(세)", color = "대륙")
save_plot(p12, "12_growth_coupling.png")

# ---------------------------------------------------------------------------
# 9. 국가 궤적 facet — 후퇴 국가 강조
# ---------------------------------------------------------------------------
hr("9. 국가 궤적 (대륙 facet, 후퇴국 강조)")
decliners <- gr %>% filter(le_gain < 0) %>% pull(country)
cat("55년간 기대수명이 후퇴한 국가:", paste(decliners, collapse=", "), "\n")
gap_hl <- gap %>% mutate(후퇴 = country %in% decliners)
p13 <- ggplot(gap_hl, aes(year, lifeExp, group = country)) +
  geom_line(data = filter(gap_hl, !후퇴), color = "grey80", alpha = 0.6) +
  geom_line(data = filter(gap_hl, 후퇴), color = "#D55E00", linewidth = 0.9) +
  facet_wrap(~continent, nrow = 1) +
  labs(title = "국가별 기대수명 궤적 (주황=55년간 후퇴한 국가)",
       subtitle = "에이즈·분쟁이 만든 1990년대 급락이 개별 선으로 드러남",
       x = "연도", y = "기대수명(세)")
save_plot(p13, "13_facet_decliners.png", w = 12, h = 4.5)

# ---------------------------------------------------------------------------
# 10. 핵심 발견 + 재현 정보
# ---------------------------------------------------------------------------
hr("핵심 발견 (Key findings)")
cat("  1) 분포: gdpPercap·pop 강한 우편향 → 로그/중앙값. '이봉형'은 풀링 착시(§3).\n")
cat("  2) 인구가중 ≠ 비가중: '평균적 사람'의 수명은 '평균적 국가'와 다르다(§5).\n")
cat("  3) 수렴: 기대수명은 σ·β 모두 수렴, 소득은 상대적으로 완만(§6).\n")
cat("  4) 관계: lifeExp~log(GDP) R²≈", round(summary(fit_all)$r.squared,2),
    "— 로그형 수확체감. 연도별로 강도 변화(§7).\n")
cat("  5) 잔차: 부유하지만 단명한 석유경제 vs 소득 대비 장수국이 명확히 분리(§7).\n")
cat("  6) 후퇴국: 소수지만 존재 — 성장이 자동 보장이 아님(§8–9).\n")

writeLines(capture.output(sessionInfo()), file.path(figdir, "session_info.txt"))
cat(sprintf("\n→ figures/ 차트 13종 + session_info.txt 저장 완료. (%s)\n",
            format(Sys.time(), "%Y-%m-%d %H:%M")))
