(bz/hook svelte-mode-hook bz/svelte-setup
  (setq qvk-snippets
        '(("<RET>" "<script lang=\"ts\">\n</script>\n\n<style>\n</style>\n")
          ("b" "<button on:click={}></button>" -9)
          ("d" "<div></div>" 5)
          ("t" (lambda () (let ((tag (read-string "Tag: "))) (insert (format "<%s></%s>" tag tag)))))
          ))

  (setq forward-sexp-function #'bz/jsx-forward-sexp))

(setq js-indent-level 4)


