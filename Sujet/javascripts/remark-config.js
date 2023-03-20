function remarkCreateWithConfig(mdFile, style)
  {
     return remark.create({sourceUrl              : mdFile     ,
                           ratio                  : '16:9'     ,
                           slideNumberFormat      : '%current%',
                           countIncrementalSlides : false      ,
                           highlightLanguage      : 'scala'    ,
                           highlightLines         : true       ,
                           highlightStyle         : style      }) ;
  }
