# levels works

    Code
      levels(sea, blanks = TRUE, outliers = TRUE)
    Output
      # A tibble: 8,064 x 8
         measurement stage well  type  group  time sensor value
               <dbl> <fct> <chr> <chr> <fct> <dbl> <fct>  <dbl>
       1           1 basal A01   blank blank     0 O2      151.
       2           1 basal A01   blank blank    14 O2      151.
       3           1 basal A01   blank blank    28 O2      151.
       4           1 basal A01   blank blank    42 O2      151.
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
      # A tibble: 7,392 x 8
         measurement stage well  type  group  time sensor value
               <dbl> <fct> <chr> <chr> <fct> <dbl> <fct>  <dbl>
       1           1 basal A01   blank blank     0 O2      151.
       2           1 basal A01   blank blank    14 O2      151.
       3           1 basal A01   blank blank    28 O2      151.
       4           1 basal A01   blank blank    42 O2      151.
       5           1 basal A01   blank blank    55 O2      152.
       6           1 basal A01   blank blank    70 O2      152.
       7           1 basal A01   blank blank    83 O2      152.
       8           1 basal A01   blank blank    97 O2      152.
       9           1 basal A01   blank blank   111 O2      152.
      10           1 basal A01   blank blank   125 O2      152.
      # i 7,382 more rows

---

    Code
      levels(sea, blanks = FALSE, outliers = FALSE)
    Output
      # A tibble: 6,216 x 8
         measurement stage well  type   group  time sensor value
               <dbl> <fct> <chr> <chr>  <fct> <dbl> <fct>  <dbl>
       1           1 basal A02   sample A         0 O2      151.
       2           1 basal A02   sample A        14 O2      150.
       3           1 basal A02   sample A        28 O2      150.
       4           1 basal A02   sample A        42 O2      150.
       5           1 basal A02   sample A        55 O2      150.
       6           1 basal A02   sample A        70 O2      150.
       7           1 basal A02   sample A        83 O2      150.
       8           1 basal A02   sample A        97 O2      150.
       9           1 basal A02   sample A       111 O2      150.
      10           1 basal A02   sample A       125 O2      150.
      # i 6,206 more rows

# rates works

    Code
      rates(sea, blanks = TRUE, outliers = TRUE, normalize = TRUE)
    Output
      # A tibble: 576 x 7
         measurement stage well  type   group rate   value
               <dbl> <fct> <chr> <chr>  <fct> <fct>  <dbl>
       1           1 basal A01   blank  blank OCR   -3.04 
       2           1 basal A02   sample A     OCR   15.0  
       3           1 basal A03   sample A     OCR   13.8  
       4           1 basal A04   sample A     OCR   16.7  
       5           1 basal A05   sample A     OCR   18.0  
       6           1 basal A06   sample B     OCR   26.7  
       7           1 basal B01   sample B     OCR   30.2  
       8           1 basal B02   sample B     OCR   43.2  
       9           1 basal B03   sample B     OCR   50.1  
      10           1 basal B04   blank  blank OCR    0.969
      # i 566 more rows

---

    Code
      rates(sea, blanks = FALSE, outliers = TRUE, normalize = TRUE)
    Output
      # A tibble: 492 x 7
         measurement stage well  type   group rate  value
               <dbl> <fct> <chr> <chr>  <fct> <fct> <dbl>
       1           1 basal A02   sample A     OCR    15.0
       2           1 basal A03   sample A     OCR    13.8
       3           1 basal A04   sample A     OCR    16.7
       4           1 basal A05   sample A     OCR    18.0
       5           1 basal A06   sample B     OCR    26.7
       6           1 basal B01   sample B     OCR    30.2
       7           1 basal B02   sample B     OCR    43.2
       8           1 basal B03   sample B     OCR    50.1
       9           1 basal B05   sample C     OCR    61.0
      10           1 basal B06   sample C     OCR    39.0
      # i 482 more rows

---

    Code
      rates(sea, blanks = TRUE, outliers = FALSE, normalize = TRUE)
    Output
      # A tibble: 528 x 7
         measurement stage well  type   group rate   value
               <dbl> <fct> <chr> <chr>  <fct> <fct>  <dbl>
       1           1 basal A01   blank  blank OCR   -3.04 
       2           1 basal A02   sample A     OCR   15.0  
       3           1 basal A03   sample A     OCR   13.8  
       4           1 basal A04   sample A     OCR   16.7  
       5           1 basal A05   sample A     OCR   18.0  
       6           1 basal A06   sample B     OCR   26.7  
       7           1 basal B01   sample B     OCR   30.2  
       8           1 basal B02   sample B     OCR   43.2  
       9           1 basal B03   sample B     OCR   50.1  
      10           1 basal B04   blank  blank OCR    0.969
      # i 518 more rows

---

    Code
      rates(sea, blanks = TRUE, outliers = TRUE, normalize = FALSE)
    Output
      # A tibble: 576 x 7
         measurement stage well  type   group rate   value
               <dbl> <fct> <chr> <chr>  <fct> <fct>  <dbl>
       1           1 basal A01   blank  blank OCR   -3.04 
       2           1 basal A02   sample A     OCR   15.0  
       3           1 basal A03   sample A     OCR   13.8  
       4           1 basal A04   sample A     OCR   16.7  
       5           1 basal A05   sample A     OCR   18.0  
       6           1 basal A06   sample B     OCR   26.7  
       7           1 basal B01   sample B     OCR   30.2  
       8           1 basal B02   sample B     OCR   43.2  
       9           1 basal B03   sample B     OCR   50.1  
      10           1 basal B04   blank  blank OCR    0.969
      # i 566 more rows

