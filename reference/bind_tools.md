# Bind tools to a config (provider-agnostic)

Bind tools to a config (provider-agnostic)

## Usage

``` r
bind_tools(config, tools, tool_choice = NULL)
```

## Arguments

- config:

  llm_config

- tools:

  list of tools (each with name, description, and
  parameters/input_schema)

- tool_choice:

  optional tool_choice spec (provider-specific shape)

## Value

modified llm_config
