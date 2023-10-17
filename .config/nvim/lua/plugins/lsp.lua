local lsp = require('lsp-zero').preset({
	name = 'minimal',
	sign_icons = {},
	suggest_lsp_servers = true,
})

local cmp = require('cmp')
local luasnip = require('luasnip')
local cmp_mappings = lsp.defaults.cmp_mappings {
	['<Cr>'] = cmp.mapping.confirm({select = true}),
	['<C-Space>'] = cmp.mapping.complete(),
	['<Tab>'] = cmp.mapping(function(fallback)
		if luasnip.jumpable(1) then
			luasnip.jump(1)
		else
			fallback()
		end
	end, {'i', 's'}),
	['<S-Tab>'] = cmp.mapping(function(fallback)
		if luasnip.jumpable(-1) then
			luasnip.jump(-1)
		else
			fallback()
		end
	end, {'i', 's'}),
}
lsp.setup_nvim_cmp({mapping = cmp_mappings})

lsp.on_attach(require('plugins.lsp.on_attach'))

lsp.setup_servers({
	'clangd',
	'idris2_lsp',
	'lua_ls',
	'pylsp',
})
lsp.skip_server_setup({
	'hls',  -- Haskell
	'rust_analyzer',
	'lean-language-server',  -- Lean 3
	'ltex',  -- Spelling/grammar
})

local lspconfig = require('lspconfig')
lspconfig.lua_ls.setup(lsp.nvim_lua_ls({
	telemetry = {enable = false}
}))
lspconfig.texlab.setup({
	settings = {
		texlab = {
			chktex = {
				onOpenAndSave = true,
				onEdit = true,
			},
		}
	}
})

lsp.setup()
cmp.setup({ completion = {autocomplete = false} })
