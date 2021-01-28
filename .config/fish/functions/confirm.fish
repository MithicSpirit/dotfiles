function confirm
    while true
        read -lP 'Do you want to continue? [y/N] ' -n 1 confirm

        switch $confirm
            case Y y
                return 0
            case '' N n
                return 1
            case '*'
                echo 'Please respond with "y" or "n"'
        end
    end
end
