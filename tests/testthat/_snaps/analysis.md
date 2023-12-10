# levels works

    Code
      levels(sea, blanks = TRUE, outliers = TRUE)
    Output
      # A tibble: 8,064 x 8
         measurement stage well  type  group  time sensor value
               <dbl> <chr> <chr> <chr> <fct> <dbl> <fct>  <dbl>
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
      # A tibble: 7,728 x 8
         measurement stage well  type  group  time sensor value
               <dbl> <chr> <chr> <chr> <fct> <dbl> <fct>  <dbl>
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
      # i 7,718 more rows

---

    Code
      levels(sea, blanks = FALSE, outliers = FALSE)
    Output
      # A tibble: 6,720 x 8
         measurement stage well  type   group  time sensor value
               <dbl> <chr> <chr> <chr>  <fct> <dbl> <fct>  <dbl>
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
      # i 6,710 more rows

