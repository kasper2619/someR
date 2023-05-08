library(openai)

Sys.setenv(
  OPENAI_API_KEY = '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'
)

Sys.getenv("OPENAI_API_KEY")

create_completion(
  model = "ada",
  prompt = "Generate a question and an answer"
)
