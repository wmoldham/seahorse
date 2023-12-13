# outliers accessor works

    Code
      outliers(sea)
    Output
      # A tibble: 4 x 2
        rate  well 
        <fct> <chr>
      1 OCR   B05  
      2 OCR   D04  
      3 OCR   D05  
      4 OCR   D06  

# outlier assignment

    Code
      outliers(sea)
    Output
      # A tibble: 1 x 2
        rate  well 
        <fct> <chr>
      1 OCR   A01  

---

    Code
      outliers(sea)
    Output
      # A tibble: 0 x 2
      # i 2 variables: rate <fct>, well <chr>

---

    Code
      outliers(sea)
    Output
      # A tibble: 1 x 2
        rate  well 
        <fct> <chr>
      1 OCR   A02  

---

    Code
      outliers(sea)
    Output
      # A tibble: 0 x 2
      # i 2 variables: rate <fct>, well <chr>

---

    Code
      outliers(sea)
    Output
      # A tibble: 1 x 2
        rate  well 
        <fct> <chr>
      1 OCR   A01  

---

    Code
      outliers(sea)
    Output
      # A tibble: 6 x 2
        rate  well 
        <fct> <chr>
      1 OCR   B04  
      2 OCR   B05  
      3 OCR   C03  
      4 OCR   D04  
      5 OCR   D05  
      6 OCR   D06  

