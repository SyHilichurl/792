# 792

remotes::install_github("SyHilichurl//792", subdir = "checkalign")
source("https://raw.githubusercontent.com/SyHilichurl/792/main/checkalign/tests/test_checkAlign.R")

### Updates
1. PNG issues
- Not need to set the width and height of the layout viewport  
- Not need to set the width and height of png plot for each facet
- Fit different n2mfrow by *grid.raster(img0,interpolate=FALSE,height=unit(1,"npc"), width=unit(1, "npc"), vp=cvp)*
- Can directly use the original png plot, faster
- Use the original *info*
  - Judge *include/exclude*
  - Draw blue hints for alignment (dividing n2mfrow)
  - Find unaligned indice and use them for new *grid.ls*
    
##### 'png structure' 
```
plot0.png
  g (listing)
  cal info
dev.off
if (unaligned)
  plot_unaligned.png
    g (listing_new)
    if (info & listing shows unaligned)
      use info$indice & listing_new to edit
  dev.off</li>
if (aligned)
  plot_aligned.png
    img0 <- plot0.png (unlink)
    use info & listing to draw on img0
  dev.off</li>
if (unaligned)
  p1 <- plot_unaligned (unlink)
if (aligned)
  p2 <- plot_unaligned (unlink)
```
---

It seems the first fails and the second works.
```png("plot1.png")
g1
png("plot2.png")
g2
dev.off()
dev.off()
```
```png("plot1.png")
png("plot2.png")
g2
dev.off()
g1
dev.off()
```

2. *ShowInOne* argument
   facet or not

3. Help page document

---

---
   
### Fails in recordplot (not considered yet)
### Works for ggplot, trellis, plot function  
checkalign1()  
![1](https://github.com/SyHilichurl/792/assets/124640901/2c07fbea-d92b-4be6-ac2e-1d47e7eabe19)  
checkalign2()  
![2](https://github.com/SyHilichurl/792/assets/124640901/a3395c19-953b-4b4c-aa55-4c5b2f6e6a5e)  
checkalign3()  
![3](https://github.com/SyHilichurl/792/assets/124640901/14d70f54-89dc-425f-908a-d290abfe289d)  
checkalign4()  
![4](https://github.com/SyHilichurl/792/assets/124640901/2c4fe643-ac20-4d3c-bcf6-3b8a4a9bec6e)  
checkalign5()  
![5](https://github.com/SyHilichurl/792/assets/124640901/f0789096-48f2-46ee-aaf6-69a6bceb091f)  
checkalign6() ("lattice")  
![6](https://github.com/SyHilichurl/792/assets/124640901/6feec9e7-0f5f-4725-8547-d24ae35f17ba)  
checkalign7() ("function")  
![7](https://github.com/SyHilichurl/792/assets/124640901/6d8000f8-ad41-4057-a774-0c20ace848bb)  

