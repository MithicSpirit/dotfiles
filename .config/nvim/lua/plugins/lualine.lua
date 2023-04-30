require('lualine').setup({
	options = {
		icons_enabled = false,
		theme = 'auto',
		component_separators = { left = '', right = ''},
		section_separators = { left = '', right = ''},
		disabled_filetypes = { statusline = {}, winbar = {} },
		ignore_focus = {},
		always_divide_middle = true,
		globalstatus = false,
		refresh = {
			statusline = 500,
			tabline = 1000,
			winbar = 1000,
		}
	},
	sections = {
		lualine_a = {'mode'},
		lualine_b = {},
		lualine_c = {
			{'location', separator = {left = ''}, padding = {
				left = 1, right = 0
			}},
			'progress',
			'searchcount',
		},
		lualine_x = {
			'filesize',
			{'encoding', separator = {right = ''}, padding = {
				left = 1, right = 0
			}},
			'fileformat',
			'filetype',
		},
		lualine_y = {'diff', 'diagnostics'},
		lualine_z = {},
	},
	inactive_sections = {
		lualine_a = {},
		lualine_b = {},
		lualine_c = {'location'},
		lualine_x = {'filetype'},
		lualine_y = {},
		lualine_z = {},
	},
	tabline = {},
	winbar = {
		lualine_a = {},
		lualine_b = {
			{
				'buffers',
				mode = 0,
				show_filename_only = false,
				max_length = function() return vim.o.columns end,
				symbols = {
					modified = ' +',
					alternate_file = '',
					directory = '',
				}
			},
		},
		lualine_c = {},
		lualine_x = {},
		lualine_y = {'branch'},
		lualine_z = {},
	},
	inactive_winbar = {
		lualine_a = {},
		lualine_b = {},
		lualine_c = {
			{
				'buffers',
				show_filename_only = false,
				max_length = function()
					return vim.o.columns * 0.8
				end,
				symbols = {
					modified = '+',
					alternate_file = '#',
					directory = '',
				}
			},
		},
		lualine_x = {},
		lualine_y = {'branch'},
		lualine_z = {},
	},
	extensions = {},
})

-- override normal binding to use lualine indices instead
vim.keymap.set('n', '<leader><Tab>', function()
	if vim.v.count > 0 then
		require('lualine.components.buffers').buffer_jump(vim.v.count, '!')
	elseif not pcall(function() vim.cmd.edit('#') end) then
		vim.api.nvim_err_writeln('E23: No alternate file')
	end
end)
