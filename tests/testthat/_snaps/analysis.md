# levels works

    Code
      levels(sea, blanks = TRUE, outliers = TRUE)
    Output
      # A tibble: 8,064 x 8
         measurement stage well  type  group  time sensor value
               <dbl> <fct> <chr> <chr> <fct> <dbl> <fct>  <dbl>
       1           1 basal A01   blank blank     0 O2      152.
       2           1 basal A01   blank blank    14 O2      152.
       3           1 basal A01   blank blank    28 O2      152.
       4           1 basal A01   blank blank    42 O2      152.
       5           1 basal A01   blank blank    55 O2      152.
       6           1 basal A01   blank blank    70 O2      152.
       7           1 basal A01   blank blank    83 O2      152.
       8           1 basal A01   blank blank    97 O2      152.
       9           1 basal A01   blank blank   111 O2      152.
      10           1 basal A01   blank blank   125 O2      152.
      # i 8,054 more rows

---

    Code
      levels(sea, blanks = TRUE, outliers = FALSE)
    Output
      # A tibble: 6,888 x 8
         measurement stage well  type  group  time sensor value
               <dbl> <fct> <chr> <chr> <fct> <dbl> <fct>  <dbl>
       1           1 basal A01   blank blank     0 O2      152.
       2           1 basal A01   blank blank    14 O2      152.
       3           1 basal A01   blank blank    28 O2      152.
       4           1 basal A01   blank blank    42 O2      152.
       5           1 basal A01   blank blank    55 O2      152.
       6           1 basal A01   blank blank    70 O2      152.
       7           1 basal A01   blank blank    83 O2      152.
       8           1 basal A01   blank blank    97 O2      152.
       9           1 basal A01   blank blank   111 O2      152.
      10           1 basal A01   blank blank   125 O2      152.
      # i 6,878 more rows

---

    Code
      levels(sea, blanks = FALSE, outliers = FALSE)
    Output
      # A tibble: 5,544 x 8
         measurement stage well  type   group  time sensor value
               <dbl> <fct> <chr> <chr>  <fct> <dbl> <fct>  <dbl>
       1           1 basal A02   sample A         0 O2      151.
       2           1 basal A02   sample A        14 O2      151.
       3           1 basal A02   sample A        28 O2      151.
       4           1 basal A02   sample A        42 O2      151.
       5           1 basal A02   sample A        55 O2      151.
       6           1 basal A02   sample A        70 O2      151.
       7           1 basal A02   sample A        83 O2      151.
       8           1 basal A02   sample A        97 O2      151.
       9           1 basal A02   sample A       111 O2      151.
      10           1 basal A02   sample A       125 O2      151.
      # i 5,534 more rows

# rates works

    Code
      rates(sea, blanks = TRUE, outliers = TRUE, normalize = TRUE)
    Output
      # A tibble: 480 x 7
         measurement stage well  type   group rate  value
               <dbl> <fct> <chr> <chr>  <fct> <fct> <dbl>
       1           1 basal A02   sample A     OCR    13.4
       2           1 basal A03   sample A     OCR    12.2
       3           1 basal A04   sample A     OCR    15.1
       4           1 basal A05   sample A     OCR    16.4
       5           1 basal A06   sample B     OCR    25.1
       6           1 basal B01   sample B     OCR    28.7
       7           1 basal B02   sample B     OCR    41.6
       8           1 basal B03   sample B     OCR    48.5
       9           1 basal B05   sample C     OCR    59.4
      10           1 basal B06   sample C     OCR    37.4
      # i 470 more rows

---

    Code
      rates(sea, blanks = FALSE, outliers = TRUE, normalize = TRUE)
    Output
      # A tibble: 480 x 7
         measurement stage well  type   group rate  value
               <dbl> <fct> <chr> <chr>  <fct> <fct> <dbl>
       1           1 basal A02   sample A     OCR    13.4
       2           1 basal A03   sample A     OCR    12.2
       3           1 basal A04   sample A     OCR    15.1
       4           1 basal A05   sample A     OCR    16.4
       5           1 basal A06   sample B     OCR    25.1
       6           1 basal B01   sample B     OCR    28.7
       7           1 basal B02   sample B     OCR    41.6
       8           1 basal B03   sample B     OCR    48.5
       9           1 basal B05   sample C     OCR    59.4
      10           1 basal B06   sample C     OCR    37.4
      # i 470 more rows

---

    Code
      rates(sea, blanks = TRUE, outliers = FALSE, normalize = TRUE)
    Output
      # A tibble: 396 x 7
         measurement stage well  type   group rate  value
               <dbl> <fct> <chr> <chr>  <fct> <fct> <dbl>
       1           1 basal A02   sample A     OCR    13.4
       2           1 basal A05   sample A     OCR    16.4
       3           1 basal A06   sample B     OCR    25.1
       4           1 basal B01   sample B     OCR    28.7
       5           1 basal B02   sample B     OCR    41.6
       6           1 basal B03   sample B     OCR    48.5
       7           1 basal B06   sample C     OCR    37.4
       8           1 basal C01   sample C     OCR    35.7
       9           1 basal C05   sample D     OCR    58.3
      10           1 basal C06   sample D     OCR    45.4
      # i 386 more rows

---

    Code
      rates(sea, blanks = TRUE, outliers = TRUE, normalize = FALSE)
    Output
      # A tibble: 576 x 7
         measurement stage well  type   group rate   value
               <dbl> <fct> <chr> <chr>  <fct> <fct>  <dbl>
       1           1 basal A01   blank  blank OCR   -4.60 
       2           1 basal A02   sample A     OCR   13.4  
       3           1 basal A03   sample A     OCR   12.2  
       4           1 basal A04   sample A     OCR   15.1  
       5           1 basal A05   sample A     OCR   16.4  
       6           1 basal A06   sample B     OCR   25.1  
       7           1 basal B01   sample B     OCR   28.7  
       8           1 basal B02   sample B     OCR   41.6  
       9           1 basal B03   sample B     OCR   48.5  
      10           1 basal B04   blank  blank OCR   -0.591
      # i 566 more rows

