# LLMR

LLMR offers a unified interface for interacting with multiple Large Language Model APIs in R, including OpenAI, Anthropic, Groq, Together AI, DeepSeek, and Voyage AI.

## Installation

- **CRAN**:  
  ```r
  install.packages("LLMR")
  ```

- **GitHub (development version)**:
```r
remotes::install_github("asanaei/LLMR")
```


## Example Usage


### Typical Low-Level Generative Call
Below is an example demonstrating a comprehensive configuration and API call using OpenAI.

```r
library(LLMR)

# Create a configuration with more parameters
comprehensive_openai_config <- llm_config(
  provider = "openai",
  model = "gpt-4-mini",          # Changed model name
  api_key = Sys.getenv("OPENAI_KEY"),
  temperature = 1,               # Controls randomness
  max_tokens = 750,              # Maximum tokens to generate
  top_p = 1,                     # Nucleus sampling parameter
  frequency_penalty = 0.5,       # Penalizes token frequency
  presence_penalty = 0.3         # Penalizes token presence
)

# Define a more complex message
comprehensive_message <- list(
  list(role = "system", content = "You are an expert data scientist."),
  list(role = "user", content = "When will you ever use OLS?")
)

# Call the LLM with all parameters and retrieve raw JSON as an attribute
detailed_response <- call_llm(
  config = comprehensive_openai_config,
  messages = comprehensive_message,
  json = TRUE
)

# Display the generated text response
cat("Comprehensive OpenAI Response:", detailed_response, "\n")

# Access and print the raw JSON response
raw_json_response <- attr(detailed_response, "raw_json")
print(raw_json_response)
```

### Word Embedding Call with Voyage
```r

library(LLMR)

  text_input <- c("Among the vicissitudes incident to life no event could have filled me with greater anxieties than that of which the notification was transmitted by your order, and received on the 14th day of the present month. On the one hand, I was summoned by my Country, whose voice I can never hear but with veneration and love, from a retreat which I had chosen with the fondest predilection, and, in my flattering hopes, with an immutable decision, as the asylum of my declining years--a retreat which was rendered every day more necessary as well as more dear to me by the addition of habit to inclination, and of frequent interruptions in my health to the gradual waste committed on it by time. On the other hand, the magnitude and difficulty of the trust to which the voice of my country called me, being sufficient to awaken in the wisest and most experienced of her citizens a distrustful scrutiny into his qualifications, could not but overwhelm with despondence one who (inheriting inferior endowments from nature and unpracticed in the duties of civil administration) ought to be peculiarly conscious of his own deficiencies. In this conflict of emotions all I dare aver is that it has been my faithful study to collect my duty from a just appreciation of every circumstance by which it might be affected. All I dare hope is that if, in executing this task, I have been too much swayed by a grateful remembrance of former instances, or by an affectionate sensibility to this transcendent proof of the confidence of my fellow-citizens, and have thence too little consulted my incapacity as well as disinclination for the weighty and untried cares before me, my error will be palliated by the motives which mislead me, and its consequences be judged by my country with some share of the partiality in which they originated.",
                  "When it was first perceived, in early times, that no middle course for America remained between unlimited submission to a foreign legislature and a total independence of its claims, men of reflection were less apprehensive of danger from the formidable power of fleets and armies they must determine to resist than from those contests and dissensions which would certainly arise concerning the forms of government to be instituted over the whole and over the parts of this extensive country. Relying, however, on the purity of their intentions, the justice of their cause, and the integrity and intelligence of the people, under an overruling Providence which had so signally protected this country from the first, the representatives of this nation, then consisting of little more than half its present number, not only broke to pieces the chains which were forging and the rod of iron that was lifted up, but frankly cut asunder the ties which had bound them, and launched into an ocean of uncertainty.",
                  "Called upon to undertake the duties of the first executive office of our country, I avail myself of the presence of that portion of my fellow-citizens which is here assembled to express my grateful thanks for the favor with which they have been pleased to look toward me, to declare a sincere consciousness that the task is above my talents, and that I approach it with those anxious and awful presentiments which the greatness of the charge and the weakness of my powers so justly inspire. A rising nation, spread over a wide and fruitful land, traversing all the seas with the rich productions of their industry, engaged in commerce with nations who feel power and forget right, advancing rapidly to destinies beyond the reach of mortal eye -- when I contemplate these transcendent objects, and see the honor, the happiness, and the hopes of this beloved country committed to the issue and the auspices of this day, I shrink from the contemplation, and humble myself before the magnitude of the undertaking. Utterly, indeed, should I despair did not the presence of many whom I here see remind me that in the other high authorities provided by our Constitution I shall find resources of wisdom, of virtue, and of zeal on which to rely under all difficulties. To you, then, gentlemen, who are charged with the sovereign functions of legislation, and to those associated with you, I look with encouragement for that guidance and support which may enable us to steer with safety the vessel in which we are all embarked amidst the conflicting elements of a troubled world.",
                  "Unwilling to depart from examples of the most revered authority, I avail myself of the occasion now presented to express the profound impression made on me by the call of my country to the station to the duties of which I am about to pledge myself by the most solemn of sanctions. So distinguished a mark of confidence, proceeding from the deliberate and tranquil suffrage of a free and virtuous nation, would under any circumstances have commanded my gratitude and devotion, as well as filled me with an awful sense of the trust to be assumed. Under the various circumstances which give peculiar solemnity to the existing period, I feel that both the honor and the responsibility allotted to me are inexpressibly enhanced."
                  )

  # Configure the embedding API provider (example with Voyage API)
  voyage_config <- llm_config(
    provider = "voyage",
    model = "voyage-large-2",
    api_key = Sys.getenv("VOYAGE_API_KEY")
  )

  embedding_response <- call_llm(voyage_config, text_input)
  embeddings <- parse_embeddings(embedding_response)
  # Additional processing:
  embeddings |> cor() |> print()
  
```

### Word Embedding Call with Gemini
```r
library(LLMR)

# Configure Gemini embeddings
config <- llm_config(
  provider = "gemini",
  model = "text-embedding-004", 
  api_key = Sys.getenv("GEMINI_KEY")
)

# Generate embeddings
texts <- c("This is text 1", 
           "This is text 2",
           "Bears ate all the ants!")
response <- call_llm(config, texts)
embeddings <- parse_embeddings(response)

# similarity 
print (cor(embeddings) )

# hierarchical clustering of the sentences
hclust(dist(embeddings |> t())) |> plot()
```


### Example of a High-Level Call
```r
# simulate a conversation about high tax on ultra processed food

library(LLMR)

# Create a cheaper config
cheap_config <- llm_config(
  provider = "openai", #"groq",
  model = "gpt-4o", #"llama-3.3-70b-versatile",
  api_key = Sys.getenv("OPENAI_KEY"), #Sys.getenv("GROQ_KEY"),
  temperature = 1,
#  trouble_shooting = TRUE,
  max_tokens = 1000
)

# Create an OpenAI configuration for summarization (using GPT-4o)
openai_config <- llm_config(
  provider = "openai",
  model = "gpt-4o",
  api_key = Sys.getenv("OPENAI_KEY"),
  temperature = 1,
#  trouble_shooting = TRUE,
  max_tokens = 2000
)

# Create Agents with the GROQ configuration
agent_econ <- Agent$new(
  id = "Econ",
  model_config = cheap_config,
  persona = list(role = "Economist", perspective = "Market-oriented", expertise = "Economic analysis"),
)

agent_health <- Agent$new(
  id = "Health",
  model_config = cheap_config,
  persona = list(role = "Public Health Expert", perspective = "Health-first", expertise = "Epidemiology"),
)

agent_policy <- Agent$new(
  id = "Policy",
  model_config = cheap_config,
  persona = list(role = "Policy Maker", perspective = "Regulatory", expertise = "Public Policy"),
)

agent_industry <- Agent$new(
  id = "Industry",
  model_config = cheap_config,
  persona = list(role = "Industry Representative", perspective = "Business-focused", expertise = "Food Industry"),
)

# Initialize the Conversation with summarizer_config using the OpenAI summarizer configuration
conversation <- LLMConversation$new(
  topic = "High Tax on Ultra Processed Food",
  summarizer_config = list(
    llm_config = openai_config,
    ## we can override the default summarization prompt
    #prompt = "Summarize the following conversation into fewer than 500 words, preserving speaker details and main points:",
    threshold = 2000,       # Lower threshold to force summary quickly for demonstration
    summary_length = 500
  )
)

# Add agents to the conversation
conversation$add_agent(agent_econ)
conversation$add_agent(agent_health)
conversation$add_agent(agent_policy)
conversation$add_agent(agent_industry)

# Define the discussion prompt template
prompt_template <- "Discuss the potential impacts of a high tax on ultra processed food from your perspective."

# Run several rounds to accumulate memory & trigger summarization
for(i in 1:5) {
  cat("Round:", i, "\n")
  agent_id <- sample(c("Econ", "Health", "Policy", "Industry"), 1)
  conversation$converse(agent_id, prompt_template, verbose = TRUE)
  #Sys.sleep(10)
}

# Print the full conversation history to observe responses and the triggered summarization
conversation$print_history()


#### compare with full history
conversation$conversation_history_full

```

### Example of file uploads and multimodal chats

Let us create a simple `.png` image and ask ChatGPT to see if there is a joke in it or not:

```r

# create image
temp_png_path <- tempfile(fileext = ".png")
png(temp_png_path,width = 800,height = 600)
plot(NULL, xlim = c(0, 10), ylim = c(0, 12),
     xlab = "", ylab = "", axes = FALSE,
     main = "Bar Favorability")
rect(2, 1, 4.5, 10, col = "saddlebrown")
text(3.25, 5.5, "CHOCOLATE BAR", col = "white", cex = 1.25, srt = 90)
rect(5.5, 1, 8, 5, col = "lightsteelblue")
text(6.75, 3, "BAR CHART", col = "black", cex = 1.25, srt = 90)
dev.off()

# ask gpt-4.1-mini to interpret this
llm_vision_config <- llm_config(
  provider = "openai",
  model = "gpt-4.1-mini",
  api_key = Sys.getenv("OPENAI_API_KEY")
)

# Construct the multimodal message
messages_to_send <- list(
  list(
    role = "user",
    content = list(
      # This part corresponds to the text of the prompt
      list(type = "text", text = "interpret this. Is there a joke here?"),
      # This part links to the local image file to be sent
      list(type = "file", path = temp_png_path)
    )
  )
)

# Call the LLM and print the response
# The `call_llm` function will automatically handle the file processing
response <- call_llm(llm_vision_config, messages_to_send)

# Print the final interpretation from the model
cat("LLM Interpretation:\n")
cat(response)
```

#### Example output
```
This image is a humorous visual pun. It has a title "Bar Favorability" and shows two bars side by side: one is a brown rectangle labeled "CHOCOLATE BAR," and the other is a blue square labeled "BAR CHART."

The joke is that "Bar Favorability" could mean how much people favor different types of bars, but the graphic interprets it literally. Instead of comparing favorability ratings, it shows literal bars—a chocolate bar and a bar chart—as if they were competitors in a popularity contest.

So yes, there is a joke here: it plays on the double meaning of the word "bar," blending a data visualization element (a bar chart) with a chocolate bar, using the concept of favorability in a funny, unexpected way.
```

Contributions: are welcome.
