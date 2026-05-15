library(httr2)
library(jsonlite)
library(uuid)

AUTH_KEY <- "MDE5ZTJhYzQtNDQzMS03YjM3LThjNDgtOTZlY2EwNzc3YWVjOjQ2OTQ3ZTc2LTE5ZDMtNDI3Mi1hMDE0LWUyZTlmNWRhNWUwMw=="
BASE_URL <- "https://gigachat.devices.sberbank.ru/api/v1/chat/completions"
MAX_REVIEWS <- 50


get_token <- function(auth_key) {
  cat("Запрашиваю токен...\n")

  response <- request("https://ngw.devices.sberbank.ru:9443/api/v2/oauth") |>
    req_options(ssl_verifypeer = 0) |>
    req_headers(
      Authorization   = paste("Basic", auth_key),
      RqUID           = UUIDgenerate(),
      "Content-Type"  = "application/x-www-form-urlencoded",
      Accept          = "application/json"
    ) |>
    req_body_raw("scope=GIGACHAT_API_PERS") |>
    req_perform()

  if (resp_status(response) != 200) {
    stop(paste("Не удалось получить токен. Статус:", resp_status(response)))
  }

  token <- resp_body_json(response)$access_token
  cat("Токен успешно получен!\n")
  token
}


analyze_review <- function(review_text, token) {
  prompt <- paste0(
    "Инструкция: Верни только JSON без лишнего текста и без кавычек оформления кода. ",
    "Проанализируй отзыв: ", review_text,
    ". Формат: {'sentiment': '...', 'topic': '...'}"
  )

  response <- tryCatch({
    request(BASE_URL) |>
      req_options(ssl_verifypeer = 0) |>
      req_headers(
        "Content-Type" = "application/json",
        Authorization  = paste("Bearer", token)
      ) |>
      req_body_json(list(
        model    = "GigaChat",
        messages = list(list(role = "user", content = prompt))
      )) |>
      req_perform() |>
      resp_body_json()
  }, error = function(e) {
    cat(" Ошибка:", e$message, "\n")
    NULL
  })

  if (is.null(response)) return(NULL)

  raw_text  <- response$choices[[1]]$message$content
  clean_text <- trimws(gsub("```json|```", "", raw_text))

  parsed <- tryCatch(
    fromJSON(clean_text),
    error = function(e) list(sentiment = "unknown", topic = "parse_error")
  )

  c(parsed, original_review = review_text)
}


main <- function() {
  token  <- get_token(AUTH_KEY)
  data   <- read.csv("data.csv", fileEncoding = "CP1251")
  n      <- min(nrow(data), MAX_REVIEWS)
  results <- vector("list", n)

  for (i in seq_len(n)) {
    cat("Обработка отзыва №", i, "...")
    results[[i]] <- analyze_review(data$review[i], token)
    cat(" Готово\n")
  }

  write_json(results, "result.json", pretty = TRUE, auto_unbox = TRUE)
  cat("\nПрограмма завершена. Проверьте файл result.json\n")
}

main()
