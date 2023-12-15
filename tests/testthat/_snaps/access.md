# cells accessor works

    Code
      cells(harold)
    Output
      # A tibble: 24 x 2
         well    value
         <chr>   <dbl>
       1 A01     -31.5
       2 A02   11316. 
       3 A03   12416. 
       4 A04   13150. 
       5 A05   12118. 
       6 A06   12092. 
       7 B01   12800. 
       8 B02   14000. 
       9 B03   14542. 
      10 B04     -42.5
      # i 14 more rows

# cells assignment works

    Code
      cells(`cells<-`(harold, list()))
    Output
      # A tibble: 24 x 2
         well  value
         <chr> <dbl>
       1 A01       1
       2 A02       1
       3 A03       1
       4 A04       1
       5 A05       1
       6 A06       1
       7 B01       1
       8 B02       1
       9 B03       1
      10 B04       1
      # i 14 more rows

# stages accessor works

    Code
      stages(harold)
    Output
      # A tibble: 288 x 3
         measurement stage well 
               <int> <fct> <chr>
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

# stages assignment works

    Code
      stages(`stages<-`(harold, list()))
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

# wells assignment works

    Code
      wells(`wells<-`(harold, list()))
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

