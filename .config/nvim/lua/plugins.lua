local ensure_packer = function()
	local fn = vim.fn
	local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
	if fn.empty(fn.glob(install_path)) > 0 then
		fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
		vim.cmd "packadd packer.nvim"
		return true
	end
	return false
end

local packer_bootstrap = ensure_packer()

local function configure(name)
	return string.format("require('plugins.%s')", name)
end

return require('packer').startup(function(use)
	use 'wbthomason/packer.nvim'

	use {
		'nvim-telescope/telescope.nvim', tag = '0.1.1',
		requires = {
			{'nvim-lua/plenary.nvim'},  -- dep
			{'natecraddock/telescope-zf-native.nvim'},  -- better matching
			{'nvim-telescope/telescope-file-browser.nvim'},
			{  -- project jumping
				'nvim-telescope/telescope-project.nvim',
				requires = {'airblade/vim-rooter'}
			},
			{'luc-tielen/telescope_hoogle'},
			{'xiyaowong/telescope-emoji.nvim'},
		},
		config = configure('telescope'),
	}

	--use {
	--	'nordtheme/vim',
	--	as = 'nord',
	--	config = configure('nord'),
	--}
	use {
		'catppuccin/nvim',
		as = 'catppuccin',
		config = configure('catppuccin'),
	}

	use {
		'nvim-treesitter/nvim-treesitter',
		run = vim.cmd.TSUpdate,
		config = configure('treesitter'),
	}


	use {
		'VonHeikemen/lsp-zero.nvim',
		branch = 'v2.x',
		requires = {
			'neovim/nvim-lspconfig',
			'williamboman/mason.nvim',
			'williamboman/mason-lspconfig.nvim',
			'hrsh7th/nvim-cmp',
			'hrsh7th/cmp-nvim-lsp',
			'l3mon4d3/luasnip',
			-- special language support
			--{'simrat39/rust-tools.nvim'},
			--{
			--	'mrcjkb/haskell-tools.nvim',
			--	requires = {'nvim-lua/plenary.nvim'},
			--	branch = '1.x.x',
			--	config = configure('haskell-tools'),
			--},
		},
		config = configure('lsp'),
	}

	use {
		'williamboman/mason.nvim',
		run = function() pcall(vim.cmd.MasonUpdate) end,
	}

	use {
		'glacambre/firenvim',
		run = function() vim.fn['firenvim#install'](0) end,
		config = configure('firenvim'),
	}

	use {
		'whonore/Coqtail',
		config = configure('coqtail'),
	}

	use {
		'tpope/vim-surround',
		requires = {'tpope/vim-repeat'},
	}

	use {
		'phaazon/hop.nvim',
		branch = 'v2',
		config = configure('hop'),
	}

	use {
		'mbbill/undotree',
		config = configure('undotree'),
	}

	--use {
	--	'itchyny/lightline.vim',
	--	config = configure('lightline'),
	--}
	--use {
	--	'famiu/feline.nvim',
	--	requires = {'catppuccin/nvim'},
	--	config = configure('feline'),
	use {
		'nvim-lualine/lualine.nvim',
		requires = {'catppuccin'},
		config = configure('lualine'),
	}

	use {
		'lervag/vimtex',
		config = configure('vimtex'),
	}

	use {
		'airblade/vim-rooter',
		config = configure('rooter'),
	}

	use 'edwinb/idris2-vim'
	use 'editorconfig/editorconfig-vim'


	if packer_bootstrap then
		require('packer').sync()
	end
end)
