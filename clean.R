# clean.R — Gapminder 데이터 품질 확인(Data Quality Check)
# 대상: data/gapminder.csv
# 실행: Rscript clean.R
# 의존성 없음(base R). 콘솔에 품질 리포트를 출력한다.

# ---------------------------------------------------------------------------
# 0. 설정 및 로드
# ---------------------------------------------------------------------------
options(stringsAsFactors = FALSE, width = 100)

infile <- file.path("data", "gapminder.csv")
if (!file.exists(infile)) stop(sprintf("파일을 찾을 수 없습니다: %s", infile))

df <- read.csv(infile, encoding = "UTF-8")

EXPECTED_COLS <- c("country", "year", "pop", "continent", "lifeExp", "gdpPercap")

rule <- function(title) {
  cat("\n", strrep("=", 78), "\n", sep = "")
  cat("## ", title, "\n", sep = "")
  cat(strrep("=", 78), "\n", sep = "")
}
ok   <- function(msg) cat("  [OK]   ", msg, "\n", sep = "")
warn <- function(msg) cat("  [WARN] ", msg, "\n", sep = "")
info <- function(msg) cat("  [INFO] ", msg, "\n", sep = "")

issues <- 0L
flag <- function(cond, bad_msg, good_msg) {
  if (isTRUE(cond)) { warn(bad_msg); issues <<- issues + 1L }
  else ok(good_msg)
}

# ---------------------------------------------------------------------------
# 1. 구조: 행/열, 컬럼명, 타입
# ---------------------------------------------------------------------------
rule("1. 구조(Structure)")
info(sprintf("행 수: %d, 열 수: %d", nrow(df), ncol(df)))
info(sprintf("컬럼: %s", paste(names(df), collapse = ", ")))

missing_cols <- setdiff(EXPECTED_COLS, names(df))
extra_cols   <- setdiff(names(df), EXPECTED_COLS)
flag(length(missing_cols) > 0,
     sprintf("기대 컬럼 누락: %s", paste(missing_cols, collapse = ", ")),
     "기대한 6개 컬럼이 모두 존재")
if (length(extra_cols) > 0) warn(sprintf("예상치 못한 추가 컬럼: %s", paste(extra_cols, collapse = ", ")))

cat("\n  컬럼 타입:\n")
for (nm in names(df)) cat(sprintf("    %-10s : %s\n", nm, class(df[[nm]])[1]))

num_cols <- c("year", "pop", "lifeExp", "gdpPercap")
for (nm in intersect(num_cols, names(df))) {
  flag(!is.numeric(df[[nm]]),
       sprintf("'%s'가 숫자형이 아님(class=%s)", nm, class(df[[nm]])[1]),
       sprintf("'%s'는 숫자형", nm))
}

# ---------------------------------------------------------------------------
# 2. 결측치 / 빈 문자열
# ---------------------------------------------------------------------------
rule("2. 결측치(Missing values)")
na_counts <- sapply(df, function(x) sum(is.na(x)))
total_na <- sum(na_counts)
flag(total_na > 0,
     sprintf("총 %d개 NA 발견: %s", total_na,
             paste(sprintf("%s=%d", names(na_counts), na_counts), collapse = ", ")),
     "NA 없음")
char_cols <- names(df)[sapply(df, is.character)]
for (nm in char_cols) {
  empty <- sum(trimws(df[[nm]]) == "", na.rm = TRUE)
  flag(empty > 0,
       sprintf("'%s'에 빈 문자열 %d개", nm, empty),
       sprintf("'%s'에 빈 문자열 없음", nm))
}

# ---------------------------------------------------------------------------
# 3. 중복(Duplicates)
# ---------------------------------------------------------------------------
rule("3. 중복(Duplicates)")
dup_rows <- sum(duplicated(df))
flag(dup_rows > 0, sprintf("완전 중복 행 %d개", dup_rows), "완전 중복 행 없음")

if (all(c("country", "year") %in% names(df))) {
  key <- paste(df$country, df$year, sep = "|")
  dup_key <- sum(duplicated(key))
  flag(dup_key > 0,
       sprintf("(country, year) 키 중복 %d개 — 패널 데이터 무결성 위반", dup_key),
       "(country, year) 조합이 유일(패널 키 무결성 OK)")
}

# ---------------------------------------------------------------------------
# 4. 범위/유효성(Range & validity)
# ---------------------------------------------------------------------------
rule("4. 값 범위/유효성(Range & validity)")

# 4-1. year
if ("year" %in% names(df)) {
  yr <- df$year
  flag(any(yr %% 1 != 0), "year에 소수값 존재", "year는 모두 정수")
  info(sprintf("year 범위: %d ~ %d", min(yr), max(yr)))
  flag(any(yr < 1900 | yr > 2100), "year가 비현실적 범위(1900~2100 밖)", "year가 현실적 범위 내")
}

# 4-2. lifeExp: 0 < x < 120
if ("lifeExp" %in% names(df)) {
  le <- df$lifeExp
  info(sprintf("lifeExp 범위: %.2f ~ %.2f", min(le, na.rm=TRUE), max(le, na.rm=TRUE)))
  flag(any(le <= 0 | le > 120, na.rm = TRUE),
       "lifeExp에 비현실적 값(<=0 또는 >120)", "lifeExp 모두 (0,120] 범위")
}

# 4-3. pop, gdpPercap: 양수
for (nm in intersect(c("pop", "gdpPercap"), names(df))) {
  v <- df[[nm]]
  info(sprintf("%s 범위: %s ~ %s", nm,
               format(min(v, na.rm=TRUE), big.mark=",", scientific=FALSE),
               format(max(v, na.rm=TRUE), big.mark=",", scientific=FALSE)))
  flag(any(v <= 0, na.rm = TRUE),
       sprintf("%s에 0 이하 값 존재", nm),
       sprintf("%s 모두 양수", nm))
}
if ("pop" %in% names(df)) {
  nonint <- sum(df$pop %% 1 != 0, na.rm = TRUE)
  if (nonint > 0) warn(sprintf("pop에 비정수값 %d개(인구는 정수가 자연스러움)", nonint))
  else ok("pop은 모두 정수")
}

# ---------------------------------------------------------------------------
# 5. 범주형 일관성(Categorical consistency)
# ---------------------------------------------------------------------------
rule("5. 범주형 일관성(Categorical)")
if ("continent" %in% names(df)) {
  conts <- sort(unique(df$continent))
  info(sprintf("대륙(%d): %s", length(conts), paste(conts, collapse = ", ")))
  # 공백/대소문자 변형 탐지
  norm <- tolower(trimws(df$continent))
  if (length(unique(norm)) != length(unique(df$continent)))
    warn("대륙명에 대소문자/공백 변형으로 인한 중복 표기 의심")
  else ok("대륙명 표기 일관")
  known <- c("Africa","Americas","Asia","Europe","Oceania")
  unknown <- setdiff(conts, known)
  flag(length(unknown) > 0,
       sprintf("미지의 대륙값: %s", paste(unknown, collapse=", ")),
       "모든 대륙값이 표준 5개 범주 내")
}
if ("country" %in% names(df)) {
  info(sprintf("국가 수: %d", length(unique(df$country))))
  # 앞뒤 공백 탐지
  trimmed <- sum(df$country != trimws(df$country))
  flag(trimmed > 0, sprintf("국가명에 앞뒤 공백 %d개", trimmed), "국가명 앞뒤 공백 없음")
}

# 한 국가가 두 대륙에 속하는지(매핑 불일치)
if (all(c("country","continent") %in% names(df))) {
  cc <- unique(df[, c("country","continent")])
  multi <- names(which(table(cc$country) > 1))
  flag(length(multi) > 0,
       sprintf("국가-대륙 매핑 불일치: %s", paste(multi, collapse=", ")),
       "각 국가는 단일 대륙에 매핑됨")
}

# ---------------------------------------------------------------------------
# 6. 패널 균형성(Panel completeness)
# ---------------------------------------------------------------------------
rule("6. 패널 균형성(Panel balance)")
if (all(c("country","year") %in% names(df))) {
  years_all <- sort(unique(df$year))
  n_years <- length(years_all)
  per_country <- table(df$country)
  info(sprintf("관측 연도 수: %d (%s)", n_years, paste(years_all, collapse=", ")))
  unbalanced <- per_country[per_country != n_years]
  flag(length(unbalanced) > 0,
       sprintf("불균형 패널 — 연도 수가 %d가 아닌 국가 %d개: %s",
               n_years, length(unbalanced),
               paste(sprintf("%s(%d)", names(unbalanced), unbalanced), collapse=", ")),
       sprintf("균형 패널 — 모든 국가가 %d개 연도를 보유", n_years))
  # 연도 간격 일정성
  gaps <- unique(diff(years_all))
  flag(length(gaps) > 1,
       sprintf("연도 간격이 불규칙: %s", paste(gaps, collapse=", ")),
       sprintf("연도 간격 일정(%d년)", gaps[1]))
}

# ---------------------------------------------------------------------------
# 7. 이상치(Outliers) — IQR 규칙, 참고용
# ---------------------------------------------------------------------------
rule("7. 이상치 탐지(IQR 1.5x, 참고용)")
for (nm in intersect(c("lifeExp","gdpPercap","pop"), names(df))) {
  v <- df[[nm]]
  q <- quantile(v, c(.25, .75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  lo <- q[1] - 1.5*iqr; hi <- q[2] + 1.5*iqr
  n_out <- sum(v < lo | v > hi, na.rm = TRUE)
  info(sprintf("%-10s 이상치 후보 %d개 (정상범위 [%s, %s])",
               nm, n_out,
               format(round(lo), big.mark=","), format(round(hi), big.mark=",")))
}
info("※ 이상치는 오류가 아닌 실제 극단값(예: 쿠웨이트 GDP)일 수 있어 자동 제거하지 않음.")

# ---------------------------------------------------------------------------
# 8. 최종 판정
# ---------------------------------------------------------------------------
rule("최종 판정(Summary)")
if (issues == 0L) {
  cat("  ✅ 통과: 치명적 품질 문제가 발견되지 않았습니다.\n")
} else {
  cat(sprintf("  ⚠️  %d개 항목에서 점검이 필요합니다(위 [WARN] 참조).\n", issues))
}
cat(sprintf("  검사 행 수: %d, 검사 일시: %s\n", nrow(df), format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("\n")
