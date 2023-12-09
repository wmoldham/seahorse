# init_wells works

    Code
      init_wells(wells = list(), x = sea)
    Output
      # A tibble: 24 x 3
         well  type   group
         <chr> <chr>  <fct>
       1 A01   blank  blank
       2 A02   sample A    
       3 A03   sample A    
       4 A04   sample A    
       5 A05   sample A    
       6 A06   sample B    
       7 B01   sample B    
       8 B02   sample B    
       9 B03   sample B    
      10 B04   blank  blank
      # i 14 more rows

---

    Code
      init_wells(wells = list(well = "A01", type = "sample", group = "A"))
    Output
      # A tibble: 1 x 3
        well  type   group
        <chr> <chr>  <fct>
      1 A01   sample A    

# init_stages works

    Code
      init_stages(stages = list(), x = sea)
    Output
      # A tibble: 288 x 3
         measurement stage well 
               <dbl> <chr> <chr>
       1           1 basal A01  
       2           1 basal A02  
       3           1 basal A03  
       4           1 basal A04  
       5           1 basal A05  
       6           1 basal A06  
       7           1 basal B01  
       8           1 basal B02  
       9           1 basal B03  
      10           1 basal B04  
      # i 278 more rows

# blanks formatted correctly

    Code
      sea@blanks
    Output
      # A tibble: 6 x 2
        rate  well 
        <fct> <chr>
      1 OCR   A01  
      2 OCR   B04  
      3 OCR   C03  
      4 ECAR  B04  
      5 ECAR  C03  
      6 ECAR  D06  

# outliers formatted correctly

    Code
      sea@outliers
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

