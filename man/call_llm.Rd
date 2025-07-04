% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LLMR.R
\name{call_llm}
\alias{call_llm}
\title{Call LLM API}
\usage{
call_llm(config, messages, verbose = FALSE, json = FALSE)
}
\arguments{
\item{config}{An `llm_config` object created by `llm_config()`.}

\item{messages}{Either
\itemize{
  \item a bare character vector (each element becomes a `"user"` message);
  \item a **named** character vector whose names are taken as `role`s,
        e.g.\ \code{c(system = "Be concise.", user = "Hi")};
  \item the classical list-of-lists with explicit \code{role}/\code{content}.
}
For multimodal requests the \code{content} field of a message can itself be
a list of parts, e.g.\ \code{list(type = "file", path = "image.png")}.}

\item{verbose}{Logical. If `TRUE`, prints the full API response.}

\item{json}{Logical. If `TRUE`, the returned text will have the raw JSON response
and the parsed list as attributes.}
}
\value{
The generated text response or embedding results. If `json=TRUE`,
  attributes `raw_json` and `full_response` are attached.
}
\description{
Sends a message to the specified LLM API and retrieves the response.
}
\examples{
\dontrun{
  cfg <- llm_config("openai", "gpt-4o-mini", Sys.getenv("OPENAI_API_KEY"))

  # 1. Bare string
  call_llm(cfg, "What is prompt engineering?")

  # 2. Named character vector (quick system + user)
  call_llm(cfg, c(system = "Be brief.", user = "Summarise the Kuznets curve."))

  # 3. Classic list form (still works)
  call_llm(cfg, list(list(role = "user", content = "Hello!")))

  # 4. Multimodal (vision-capable model required)
  multi_cfg <- llm_config("openai", "gpt-4o", Sys.getenv("OPENAI_API_KEY"))
  msg <- list(list(role = "user", content = list(
           list(type = "text", text = "Describe this picture"),
           list(type = "file", path = "path/to/image.png"))))
  call_llm(multi_cfg, msg)
}
}
