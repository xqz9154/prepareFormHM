# install.packages("usethis")
library(usethis)
library(gitcreds)
use_git_config(user.name = "xqz9154", user.email ="aimanqiankun55@live.com")
use_git()
create_github_token()

# save 
gitcreds::gitcreds_set()

use_github()
