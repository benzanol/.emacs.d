(bz/require fci)


(setq lsp-dart-line-length 100
      lsp-dart-flutter-widget-guides nil
      lsp-dart-flutter-fringe-colors nil
      lsp-dart-closing-labels nil)

(defun bz/dart-format-buffer ()
  (interactive)
  (bz/save-position
   (shell-command-on-region
    (point-min) (point-max)
    (format "dart format -l %s" lsp-dart-line-length)
    nil 'replace)))

(setf (alist-get 'dart-mode bz/indent-command-alist) nil)

(bz/hook dart-mode-hook bz/dart-mode-setup
  (setq-local fci-rule-column lsp-dart-line-length)
  (fci-mode)

  (hs-minor-mode)
  (bz/hs-hide-all))

(bz/keys dart-mode-map
  "C-c C-1" (@ bz/dart-remove-widget (bz/lsp-action-by-name "Remove this widget" "Remove unnecessary 'Container'"))
  "C-c C-2" (@ bz/dart-add-const (bz/lsp-action-by-name "Add 'const' modifier"))
  "C-c C-3" (@ bz/dart-expression-body (bz/lsp-action-by-name "Convert to expression body"))
  "C-c C-4" (@ bz/dart-block-body (bz/lsp-action-by-name "Convert to block body"))
  "C-c C-5" (@ bz/dart-extract-widget (bz/lsp-action-by-name "Extract Widget")))

(font-lock-add-keywords
 'dart-mode
 ;; The ! operator is dangerous!
 `(("\\(!\\)[^=]" (1 'error))
   (" \\(=>\\) " (1 'font-lock-keyword-face))
   ;; ("\\(\\(?:([a-zA-Z0-9, ]*) *\\)?=>\\)" (1 'font-lock-keyword-face))
   ))


;;; Snippets

(setf (alist-get 'dart-mode bz/snippet-mode-alist)
      '(
        ("w b" "Builder(\nbuilder: (context) => <<>>,\n)")
        ("w B" "StatefulBuilder(\nbuilder: (context, setState) => <<>>,\n)")
        ("w c" "Container(\nchild: <<>>,\n)")
        ("w C" "Center(\nchild: <<>>,\n)")
        ("w g" "GestureDetector(\nonTap: () {},\nchild: <<>>,\n)")
        ("w o" "Transform.translate(\noffset: Offset(0, 0),\nchild: <<>>,\n)")
        ("w r" "Row(\nchildren: [\n<<>>\n],\n)")
        ("w R" "Column(\nchildren: [\n<<>>\n],\n)")
        ("w p" "Padding(\npadding: const EdgeInsets.all(0),\nchild: <<>>,\n)")
        ("w s" "Scaffold(\nappBar: AppBar(\ntitle: const Text('Title'),\n),\nbody: <<>>,\n)")
        ("w t" "Text('<<>>')")
        ("w T" "const Text('<<>>')")
        ("W" "<<:name>>(\nchild: <<>>,\n)")

        ("p a" "mainAxisAlignment: MainAxisAlignment.<<>>,")
        ("p A" "crossAxisAlignment: CrossAxisAlignment.<<>>,")
        ("p b" "decoration: BoxDecoration(border: Border.all(color: Colors.<<>>)),")
        ("p p" "padding: EdgeInsets.all(<<>>),")
        ("p s" "style: Theme.of(context).<<>>,")

        ("r a" "final data = ModalRoute.of(context).settings.arguments;")
        ("r p" "Navigator.pushNamed(context, '/<<>>/');")

        ("g c" "Consumer<<<:model>>>(\nbuilder: (context, model, child) => <<>>\n),")
        ("g o" "Provider.of<<<>>>(context, listen: false)")


        ("i m" "import 'package:flutter/material.dart';")

        ("u i" ["final TextEditingController _<<:name>> = TextEditingController();"
                ""
                "@override"
                "void dispose() {"
                "  _<<:name>>.dispose();"
                "  super.dispose();"
                "}"])

        ;; Stateless widget
        ("c w" ["class <<:name>> extends StatelessWidget {"
                "  const <<:name>>({super.key});"
                ""
                "  @override"
                "  Widget build(BuildContext context) {"
                "    return <<>>;"
                "  }"
                "}"])

        ;; Stateful widget
        ("c s" ["class <<:name>> extends StatefulWidget {"
                "  const <<:name>>({super.key});"
                ""
                "  @override"
                "  State<<<:name>>> createState() => _<<:name>>State();"
                "}"
                ""
                "class _<<:name>>State extends State<<<:name>>> {"
                "  @override"
                "  Widget build(BuildContext context) {"
                "    return <<>>;"
                "  }"
                "}"])
        ))
