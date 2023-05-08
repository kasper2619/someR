library(openai)

Sys.setenv(
  OPENAI_API_KEY = ''
)

Sys.getenv("OPENAI_API_KEY")

create_completion(
  model = "ada",
  prompt = "Generate a question and an answer"
)
