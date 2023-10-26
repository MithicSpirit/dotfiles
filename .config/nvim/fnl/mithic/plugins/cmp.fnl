[{1 "hrsh7th/nvim-cmp"
  :dependencies ["saadparwaiz1/cmp_luasnip" "hrsh7th/cmp-nvim-lsp"]
  :lazy false
  :config #(let [cmp (require :cmp)
                 luasnip (require :luasnip)]
            (cmp.setup
              {:snippet {:expand
                         (fn [args] (luasnip.lsp_expand args.body))}
               :mapping
                 {"<C-Space>" (cmp.mapping.complete)
                  "<C-b>" (cmp.mapping.scroll_docs (- 4))
                  "<C-f>" (cmp.mapping.scroll_docs 4)
                  "<Cr>" (cmp.mapping
                          (fn [fallback]
                            (if (cmp.visible) (cmp.confirm {:select true})
                                (luasnip.jumpable 1) (luasnip.jump 1)
                                (fallback))))
                  "<C-c>" (cmp.mapping.abort)
                  "<Tab>" (cmp.mapping
                           (fn [fallback]
                             (if (cmp.visible) (cmp.select_next_item)
                                 (luasnip.jumpable 1) (luasnip.jump 1)
                                 (fallback)))
                           [:i :s])
                  "<S-Tab>" (cmp.mapping
                             (fn [fallback]
                               (if (cmp.visible) (cmp.select_prev_item)
                                   (luasnip.jumpable (- 1)) (luasnip.jump (- 1))
                                   (fallback)))
                             [:i :s])
                  "<C-n>" (cmp.mapping
                           (fn [fallback]
                             (if (cmp.visible) (cmp.select_next_item)
                                 (fallback))))
                  "<C-p>" (cmp.mapping
                           (fn [fallback]
                             (if (cmp.visible) (cmp.select_prev_item)
                                 (fallback))))}
               :sources [{:name :nvim_lsp} {:name :luasnip} {:name :buffer}]
               :completion {:autocomplete false}}))}


 {1 "saadparwaiz1/cmp_luasnip"
  :dependencies ["L3MON4D3/LuaSnip"]}

 {1 "hrsh7th/cmp-nvim-lsp"}


 {1 "L3MON4D3/LuaSnip"}]
