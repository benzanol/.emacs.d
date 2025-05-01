(setq deepseek-api-key (getenv "DEEPSEEK_KEY"))

(defun deepseek-api-query (prompt)
  "Send PROMPT to DeepSeek API and insert response."
  (interactive "sPrompt: ")
  (let ((url-request-method "POST")
        (url-request-extra-headers
         `(("Content-Type" . "application/json")
           ("Authorization" . ,(concat "Bearer " deepseek-api-key))))
        (url-request-data (json-encode `(("prompt" . ,prompt)))))
    (url-retrieve "https://api.deepseek.com/v1/chat/completions"
                  (lambda (status)
                    (message "%s", status))

                  )))

(deepseek-api-query "Respond with just the message 'hello world!', no extra words.")

;; curl https://api.deepseek.com/chat/completions \
;;   -H "Content-Type: application/json" \
;;   -H "Authorization: Bearer %s" \
;;   -d '{
;;         "model": "deepseek-chat",
;;         "messages": [
;;           {"role": "system", "content": "You are an obedient assistant."},
;;           {"role": "user", "content": "Respond with just the message Hello World"}
;;         ],
;;         "stream": false
;;       }'
