function py
	if test (count $argv) = 0
		ipython $argv
	else
		python $argv
	end
end
