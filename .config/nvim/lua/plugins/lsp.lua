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

lsp.on_attach(function(client, buffer)
	local opts = {buffer = buffer, remap = false}

	vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
	vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
	vim.keymap.set('n', '<leader>la', vim.lsp.buf.code_action, opts)
	vim.keymap.set('n', '<leader>lr', vim.lsp.buf.rename, opts)
	vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
	vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
end)

lsp.setup_servers({
	'clangd',
	'digestif',  -- LaTeX
	'hls',  -- Haskell
	'idris2_lsp',
	'lua_ls',
	'pylsp',
	'rust_analyzer',
})
lsp.nvim_workspace()

lsp.setup()

cmp.setup({ completion = {autocomplete = false} })
