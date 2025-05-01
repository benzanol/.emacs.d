(qv/package gpt)

(setq gpt-openai-key (getenv "OPENAI_KEY"))
(setq gpt-openai-engine "code-davinci-002")
(setq gpt-bearer (getenv "OPENAI_BEARER"))

(insert (format " curl https://api.openai.com/v1/engines/davinci-codex/completions \
     -H \"Content-Type: application/json\" \
     -H \"Authorization: Bearer %s\" \
     -d '{\"prompt\": \"Hello, Chat GPT!\", \"max_tokens\": 50, \"n\": 1, \"stop\": \"\\n\"}'"
                openai-bearer))

;; curl https://api.openai.com/v1/engines/davinci-codex/completions -H "Content-Type: application/json" -H "Authorization: Bearer %s" -d '{"prompt": "Hello, Chat GPT!", "max_tokens": 50, "n": 1, "stop": "\n"}'

