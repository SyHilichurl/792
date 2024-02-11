# 792

remotes::install_github("SyHilichurl//792", subdir = "checkalign")
source("https://raw.githubusercontent.com/SyHilichurl/792/main/checkalign/tests/test_checkAlign.R")

### Works for ggplot
checkalign1()
![11](https://github.com/SyHilichurl/792/assets/124640901/2c07fbea-d92b-4be6-ac2e-1d47e7eabe19)
checkalign2()
![ggplot](https://github.com/SyHilichurl/792/assets/124640901/a3395c19-953b-4b4c-aa55-4c5b2f6e6a5e)
checkalign3()
![33](https://github.com/SyHilichurl/792/assets/124640901/2c4fe643-ac20-4d3c-bcf6-3b8a4a9bec6e)
checkalign4()
![44](https://github.com/SyHilichurl/792/assets/124640901/f0789096-48f2-46ee-aaf6-69a6bceb091f)

### Failures
checkalign5() ("lattice", once works, unrepeatable)
![lattice](https://github.com/SyHilichurl/792/assets/124640901/6feec9e7-0f5f-4725-8547-d24ae35f17ba)

### Issues
- Bugs
  *png("plot2.png")
  png("plot0.png") / trellis.device(..., "plot0.png", ...)
  dev.off()
  dev.off()*
- *png("plot0.png")* and *unlink("plot0.png")* each time
- Cannot use *png("plot0.png")* once, or not fit
- Tried examples
  ![failure1](https://github.com/SyHilichurl/792/assets/124640901/d66c8d94-0728-4b61-9f45-c57db8f89666)
  ![failure2](https://github.com/SyHilichurl/792/assets/124640901/f41ca83e-fec7-45a5-a1cd-2a788a231f6b)
  ![failure3](https://github.com/SyHilichurl/792/assets/124640901/4a28bd1e-7cdb-4360-b737-f1e2a337f02a)

