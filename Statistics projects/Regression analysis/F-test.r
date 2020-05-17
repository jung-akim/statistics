load('youtube.RData')
full.model = lm(views~.,data=youtube)
anova(full.model)
r = 8
n = nrow(youtube)
k = ncol(youtube[,-1])
SST = var(youtube$views)*(n-1)
SSRes.full = sum(residuals(full.model)^2)
SSR.full = SST - SSRes.full
reduced.model = lm(views~.-publish.hour-tag_appeared_in_title_count-tags.count-title.length-description.length-caps-exclamation-question,data=youtube)
SSRes.reduced = sum(residuals(reduced.model)^2)
SSR.reduced = SST - SSRes.reduced
MSRes.full = SSRes.full/(n-k-1)
F.test = (SSR.full - SSR.reduced)/r/MSRes.full
pf(F.test,r,n-k-1,lower.tail = F)#0.001336476
# With p-value = 0.001336476 < 0.01, we conclude that at least
# one of the 8 predictors; the published hour, the number of tags included in the title,
# the number of tags, the title length, the description length, whether the number of
# uppercase letters exceed the number of lowercase letters, whether exclamation mark is included,
# and whether question mark is included in the title has significant influence on the number of views
# given that all the other variables that the author cannot control are included in the model.