# outliers accessor works

    Code
      outliers(sea)
    Output
      # A tibble: 576 x 5
         rate  measurement group well  outlier
         <fct>       <dbl> <fct> <chr> <lgl>  
       1 OCR             1 blank A01   FALSE  
       2 OCR             1 A     A02   FALSE  
       3 OCR             1 A     A03   FALSE  
       4 OCR             1 A     A04   FALSE  
       5 OCR             1 A     A05   FALSE  
       6 OCR             1 B     A06   FALSE  
       7 OCR             1 B     B01   FALSE  
       8 OCR             1 B     B02   FALSE  
       9 OCR             1 B     B03   FALSE  
      10 OCR             1 blank B04   FALSE  
      # i 566 more rows

# outlier assignment

    Code
      outliers(sea)
    Output
      # A tibble: 576 x 5
         rate  measurement group well  outlier
         <fct>       <dbl> <fct> <chr> <lgl>  
       1 OCR             1 blank A01   TRUE   
       2 OCR             1 A     A02   FALSE  
       3 OCR             1 A     A03   FALSE  
       4 OCR             1 A     A04   FALSE  
       5 OCR             1 A     A05   FALSE  
       6 OCR             1 B     A06   FALSE  
       7 OCR             1 B     B01   FALSE  
       8 OCR             1 B     B02   FALSE  
       9 OCR             1 B     B03   FALSE  
      10 OCR             1 blank B04   FALSE  
      # i 566 more rows

---

    Code
      outliers(`outliers<-`(sea, "remove"))
    Output
      # A tibble: 576 x 5
         rate  measurement group well  outlier
         <fct>       <dbl> <fct> <chr> <lgl>  
       1 OCR             1 blank A01   FALSE  
       2 OCR             1 A     A02   FALSE  
       3 OCR             1 A     A03   FALSE  
       4 OCR             1 A     A04   FALSE  
       5 OCR             1 A     A05   FALSE  
       6 OCR             1 B     A06   FALSE  
       7 OCR             1 B     B01   FALSE  
       8 OCR             1 B     B02   FALSE  
       9 OCR             1 B     B03   FALSE  
      10 OCR             1 blank B04   FALSE  
      # i 566 more rows

---

    Code
      outliers(sea)
    Output
      # A tibble: 576 x 5
         rate  measurement group well  outlier
         <fct>       <dbl> <fct> <chr> <lgl>  
       1 OCR             1 blank A01   TRUE   
       2 OCR             1 A     A02   TRUE   
       3 OCR             1 A     A03   FALSE  
       4 OCR             1 A     A04   FALSE  
       5 OCR             1 A     A05   FALSE  
       6 OCR             1 B     A06   FALSE  
       7 OCR             1 B     B01   FALSE  
       8 OCR             1 B     B02   FALSE  
       9 OCR             1 B     B03   FALSE  
      10 OCR             1 blank B04   FALSE  
      # i 566 more rows

---

    Code
      outliers(sea)
    Output
      # A tibble: 576 x 5
         rate  measurement group well  outlier
         <fct>       <dbl> <fct> <chr> <lgl>  
       1 OCR             1 blank A01   TRUE   
       2 OCR             1 A     A02   FALSE  
       3 OCR             1 A     A03   FALSE  
       4 OCR             1 A     A04   FALSE  
       5 OCR             1 A     A05   FALSE  
       6 OCR             1 B     A06   FALSE  
       7 OCR             1 B     B01   FALSE  
       8 OCR             1 B     B02   FALSE  
       9 OCR             1 B     B03   FALSE  
      10 OCR             1 blank B04   FALSE  
      # i 566 more rows

---

    Code
      outliers(sea)
    Output
      # A tibble: 576 x 5
         rate  measurement group well  outlier
         <fct>       <dbl> <fct> <chr> <lgl>  
       1 OCR             1 blank A01   TRUE   
       2 OCR             1 A     A02   FALSE  
       3 OCR             1 A     A03   FALSE  
       4 OCR             1 A     A04   FALSE  
       5 OCR             1 A     A05   FALSE  
       6 OCR             1 B     A06   FALSE  
       7 OCR             1 B     B01   FALSE  
       8 OCR             1 B     B02   FALSE  
       9 OCR             1 B     B03   FALSE  
      10 OCR             1 blank B04   FALSE  
      # i 566 more rows

