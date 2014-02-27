# Install dependencies
for (p in c('devtools','sqldf')){
  
  if (!require(p, character.only=T)){
    
    install.packages(p)
    require(p, character.only=T)
    
  } 
}


# For production version, use github.user = 'ohi-science'
# For development version, use github.user = 'bbest'

github.user = 'ohi-science'

install_github('rCharts', github.user)
install_github('ohicore', github.user)

install_github('ohigui' , github.user)


# Launch application
ohigui::launchApp()
