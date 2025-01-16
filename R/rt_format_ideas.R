format_scientific <- function(x) {
    formatted <- sapply(x, function(num) {
        # Handle zero separately to avoid log10 issues
        if (num == 0) {
            # Zero would be " 0   e+ 0"
            return(" 0   e+ 0")
        }

        sign_char <- if (num < 0) "-" else " "
        abs_num <- abs(num)

        # Determine exponent (e)
        exponent <- floor(log10(abs_num))

        # Compute mantissa
        m <- abs_num / (10^exponent)

        # Round mantissa to 2 decimal places (up to 3 significant digits effectively)
        # We'll always start with a two-decimal format, then adjust.
        m_str <- sprintf("%.2f", m)
        # m_str now looks like "x.yy"
        # For example, if m=5, m_str="5.00"
        # if m=1.234..., after rounding it might be "1.23"

        # Remove trailing zeros if any:
        # If '.00' at the end, replace '.00' with '   '
        # If last char is '0', replace with ' '

        # Break m_str into parts: integer part and decimal part
        # We know m_str is always in the form: D.dd (1 digit before decimal since m in [1,10))
        int_part <- substr(m_str, 1, 1)
        dec_part <- substr(m_str, 3, 4)  # two decimal digits

        # Check if we have '.00'
        if (dec_part == "00") {
            # No decimal digits needed, all spaces
            dec_pt_char <- " "
            dec_dig1 <- " "
            dec_dig2 <- " "
        } else {
            # If not all zeros, check trailing zeros
            # If last digit of dec_part is '0', replace it with space
            if (substr(dec_part, 2, 2) == "0") {
                dec_part <- paste0(substr(dec_part, 1, 1), " ")
            }
            # After this step, dec_part might look like "20" -> "2 "
            # or "23" stays "23"

            # Put decimal point
            dec_pt_char <- "."
            dec_dig1 <- substr(dec_part, 1, 1)
            dec_dig2 <- substr(dec_part, 2, 2)

            # If decimal part ended up as '  ', it means we had a single-digit fraction
            # Actually that's still fine: "1.2 " means one decimal digit and a trailing space.
            # If dec_part == "  ", that means decimal point but no digits?
            # That won't happen because we always had 2 decimals. After trimming zeros we should have at least one digit or a space.
        }

        # Now format the exponent
        exp_sign_char <- if (exponent < 0) "-" else "+"
        exp_val <- abs(exponent)

        # If exponent is single-digit, we use a space before it
        # If double-digit, print both digits
        if (exp_val < 10) {
            # single-digit exponent
            exp_str <- paste0(" ", exp_val)
        } else {
            # possibly multi-digit exponent (up to 2 digits from examples)
            exp_str <- sprintf("%2d", exp_val) # ensures two-digit formatting
        }

        # Construct final string
        # Positions:
        # 1: sign_char
        # 2: int_part
        # 3: dec_pt_char or space
        # 4: dec_dig1 or space
        # 5: dec_dig2 or space
        # 6: 'e'
        # 7: exp_sign_char (+/-)
        # 8: first exponent char (space or digit)
        # 9: second exponent char (digit)

        final_str <- paste0(sign_char,
                            int_part,
                            dec_pt_char,
                            dec_dig1,
                            dec_dig2,
                            "e",
                            exp_sign_char,
                            substr(exp_str, 1, 1),
                            substr(exp_str, 2, 2))

        final_str
    })

    return(formatted)
}

# Example usage:
test_numbers <- c(1.234342e+12, 2.340000e+02, 5.000000e+00, -1.230000e+05,
                  1.230000e-04, 1.000000e+03, -1.000000e-06, 1.234000e+01)

format_scientific(test_numbers)



align_numbers <- function(numbers) {
    # Function to remove trailing zeros and round decimals to the second decimal digit
    remove_trailing_zeros <- function(x) {
        if (x %% 1 == 0) {
            # If it's a whole number, return it as an integer (no decimal point)
            return(as.integer(x))
        } else {
            # Round decimal numbers to 2nd decimal place
            rounded_value <- ceiling(x * 100) / 100
            # Return rounded value, removing trailing zeros
            return(as.numeric(format(rounded_value, scientific = FALSE)))
        }
    }

    # Apply the remove_trailing_zeros function to each number
    numbers <- sapply(numbers, remove_trailing_zeros)

    # Automatically detect the alignment by looking at the max width
    max_width <- max(nchar(as.character(numbers)))

    # Format the numbers based on their width
    formatted_numbers <- format(numbers, width = max_width)

    # Return the formatted numbers as a character vector
    return(formatted_numbers)
}

# Example usage:
numbers <- c(123.456, 78.9, 5.6, 0.123)
print(align_numbers(numbers, align_by = "decimal"))
print(align_numbers(numbers, align_by = "lowest"))


















align_numbers <- function(numbers) {
    # Function to remove trailing zeros and round decimals to the second decimal digit
    remove_trailing_zeros <- function(x) {
        if (x %% 1 == 0) {
            # If it's a whole number, return it as an integer (no decimal point)
            return(as.integer(x))
        } else {
            # Round decimal numbers to 2nd decimal place
            rounded_value <- ceiling(x * 100) / 100
            # Return rounded value, removing trailing zeros
            return(as.numeric(format(rounded_value, scientific = FALSE)))
        }
    }

    # Function to check if any number has more than 5 digits to the left or right of the decimal point
    is_large_number <- function(x) {
        # Check if the number has more than 5 digits to the left or right of the decimal point
        return(nchar(gsub("^-?\\d+\\.", "", as.character(abs(x)))) > 5 || nchar(gsub("^\\d+\\.", "", as.character(abs(x)))) > 5)
    }

    # Check if any number in the vector exceeds the criteria
    if (any(sapply(numbers, is_large_number))) {
        # If any number exceeds the criteria, apply scientific notation to the entire vector
        numbers <- sapply(numbers, format_scientific)
    } else {
        # If no number exceeds the criteria, process normally
        numbers <- sapply(numbers, remove_trailing_zeros)
    }

    # Automatically detect the alignment by looking at the max width
    max_width <- max(nchar(as.character(numbers)))

    # Format the numbers based on their width
    formatted_numbers <- format(numbers, width = max_width)

    # Return the formatted numbers as a character vector
    return(formatted_numbers)
}

# Example usage:
numbers <- c(123456.789, 78.9, 5.600, 0.123456, 42)
print(align_numbers(numbers))






