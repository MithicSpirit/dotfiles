local install_path = vim.fn.stdpath('data')
	.. '/site/pack/packer/start/packer.nvim'
local packer_bootstrap = vim.fn.empty(vim.fn.glob(install_path)) > 0
if packer_bootstrap then
	vim.fn.system({
		'git', 'clone', '--depth', '1',
		'https://github.com/wbthomason/packer.nvim',
		install_path
	})
	vim.cmd("packadd packer.nvim")
end

local function configure(name)
	return string.format("require('plugins.%s')", name)
end

require('packer').startup(function(use)
	use 'wbthomason/packer.nvim'

	use {
		'nvim-telescope/telescope.nvim', branch = '0.1.x',
		requires = {
			{ 'nvim-lua/plenary.nvim' }, -- dep
			{ 'natecraddock/telescope-zf-native.nvim' }, -- better matching
			{ 'nvim-telescope/telescope-file-browser.nvim' },
			{
				-- project jumping
				'nvim-telescope/telescope-project.nvim',
				--requires = {'airblade/vim-rooter'}
			},
			{ 'luc-tielen/telescope_hoogle' },
			{ 'xiyaowong/telescope-emoji.nvim' },
			{ 'nvim-telescope/telescope-dap.nvim' }
		},
		config = configure('telescope'),
	}

	--[[ use {
		'nordtheme/vim',
		as = 'nord',
		config = configure('nord'),
	} ]]
	use {
		'catppuccin/nvim',
		as = 'catppuccin',
		after = 'nvim-dap',
		config = configure('catppuccin'),
	}

	use {
		'nvim-treesitter/nvim-treesitter',
		run = function()
			require('nvim-treesitter.install').update({
				with_sync = true
			})()
		end,
		config = configure('treesitter'),
	}
	use {
		'nvim-treesitter/nvim-treesitter-textobjects',
		after = 'nvim-treesitter',
		requires = 'nvim-treesitter/nvim-treesitter',
	}
	use {
		'nvim-treesitter/nvim-treesitter-context',
		after = 'nvim-treesitter',
		requires = 'nvim-treesitter/nvim-treesitter',
		config = configure('treesitter-context'),
	}


	use {
		'VonHeikemen/lsp-zero.nvim',
		branch = 'v2.x',
		requires = {
			'neovim/nvim-lspconfig',
			'mason.nvim',
			'williamboman/mason-lspconfig.nvim',
			'hrsh7th/nvim-cmp',
			'hrsh7th/cmp-nvim-lsp',
			'l3mon4d3/luasnip',
			'telescope.nvim', -- for some keybinds
			-- special language support
			'rust-tools.nvim',
			'haskell-tools.nvim',
			'ltex_extra.nvim'
		},
		config = configure('lsp'),
	}

	use {
		'williamboman/mason.nvim',
		run = function() pcall(vim.cmd.MasonUpdate) end,
	}

	use {
		'mfussenegger/nvim-dap',
		requires = {
			'rcarriga/nvim-dap-ui',
			'telescope.nvim',
			'telescope-dap.nvim',
		},
		config = configure('dap'),
	}

	use {
		'glacambre/firenvim',
		run = function() vim.fn['firenvim#install'](0) end,
		config = configure('firenvim'),
	}

	use {
		'mikesmithgh/kitty-scrollback.nvim',
		opt = true,
		cmd = { 'KittyScrollbackGenerateKittens', 'KittyScrollbackCheckHealth' },
		tag = '*',
		config = configure('kitty-scrollback'),
	}

	use {
		'whonore/Coqtail',
		config = configure('coqtail'),
	}

	use {
		'tpope/vim-surround',
		requires = { 'tpope/vim-repeat' },
	}

	use {
		'phaazon/hop.nvim',
		branch = 'master',
		config = configure('hop'),
	}

	use {
		'justinmk/vim-sneak',
		requires = { 'tpope/vim-repeat' },
		config = configure('sneak'),
	}

	use {
		'mbbill/undotree',
		config = configure('undotree'),
	}

	use 'chrisbra/Colorizer'

	--[[ use {
		'itchyny/lightline.vim',
		config = configure('lightline'),
	} ]]
	--[[ use {
		'famiu/feline.nvim',
		requires = {'catppuccin/nvim'},
		config = configure('feline'),
	} ]]
	use {
		'nvim-lualine/lualine.nvim',
		requires = { 'catppuccin' },
		after = { 'catppuccin' },
		config = configure('lualine'),
	}

	use {
		'lervag/vimtex',
		config = configure('vimtex'),
	}

	--[[ use {
		'airblade/vim-rooter',
		config = configure('rooter'),
	} ]]

	use {
		'tpope/vim-fugitive',
		config = configure('fugitive'),
	}
	use {
		'NeogitOrg/neogit',
		requires = {
			'nvim-lua/plenary.nvim',
			'telescope.nvim',
		},
		config = configure('neogit'),
	}

	use {
		'lewis6991/gitsigns.nvim',
		config = configure('gitsigns'),
	}

	use {
		'numToStr/Comment.nvim',
		config = configure('comment'),
	}

	use 'tpope/vim-sleuth'

	use 'tpope/vim-eunuch'

	use {
		'simrat39/rust-tools.nvim',
		config = configure('rust-tools'),
	}
	use {
		'mrcjkb/haskell-tools.nvim',
		requires = { 'nvim-lua/plenary.nvim' },
		branch = '2.x.x',
		config = configure('haskell-tools'),
	}
	use {
		'Julian/lean.nvim',
		requires = {
			'neovim/nvim-lspconfig',
			'nvim-lua/plenary.nvim'
		},
		config = configure('lean')
	}
	use {
		'barreiroleo/ltex_extra.nvim',
		requires = { 'neovim/nvim-lspconfig' },
		config = configure('ltex-extra'),
	}
	use 'edwinb/idris2-vim'


	if packer_bootstrap then
		require('packer').sync()
	end
end)
