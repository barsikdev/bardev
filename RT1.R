url <- "https://raw.githubusercontent.com/barsikdev/bardev/main/RT.R"
response <- GET(url)
script_content <- content(response, "text")
cat(script_content)


#############################################################################################################################
#-----------------------------------------------Пример расчетов
data <- read.table("/Users/PUS/Downloads/INVESTMENTS\ HOME\ WORK/example.txt",
                   header=TRUE,sep=",")

rA <- data$rA
rB <- data$rB
rm <- data$rm
rf <- data$rf
#Performance Evaluation March 30, 2024 35 / 68



sA <- mean(rA-rf)/sd(rA-rf)
print(sA)

sB <- mean(rB-rf)/sd(rB-rf)
print(sB)

sm <- mean(rm-rf)/sd(rm-rf)
print(sm)

betaA <- cov(rA,rm)/var(rm)
print(betaA)

betaB <- cov(rB,rm)/var(rm)
print(betaB)

betam <- 1

tA <- mean(rA-rf)/betaA
print(tA)

tB <- mean(rB-rf)/betaB
print(tB)

tm <- mean(rm-rf)/1
print(tm)


#Alpha
alphaA <- mean(rA - (rf + betaA*(rm-rf)))
print(alphaA)

alphaB <- mean(rB - (rf + betaB*(rm-rf)))
print(alphaB)





#############################################################################################################################
install.packages("httr")
library(httr)
library(jsonlite)

send_gpt_request <- function(prompt, api_key, model_name) {
  base_url <- "https://api.openai.com/v1/chat/completions"
  body <- list(
    model = model_name,  # Убедитесь, что добавили model в тело запроса
    messages = list(list("role" = "user", "content" = prompt))  # Уточните роль и содержание
  )
  
  response <- POST(
    url = base_url,
    body = body,
    add_headers(`Authorization` = paste("Bearer", api_key)),
    encode = "json"
  )
  
  content <- content(response, "text", encoding = "UTF-8")
  return(content)
}

# важная часть
api_key <- "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"
model_name <- "gpt-4-1106-preview"
prompt <- "You are thinking about investing in a £1,000 face value bond which will mature in two years. The bond has an 8% coupon and pays interest seminannually. The current yield to maturity on similar bonds is 6%, and rates are not expected to change.
What is the bond’s price today?"

result <- send_gpt_request(prompt, api_key, model_name)
#print(result)  # Убедитесь, что здесь вы печатаете result

# Извлечение контента ответа ассистента
result_json <- fromJSON(result)
assistant_content <- result_json$choices$message$content[[1]]

# Вывод ответа ассистента на консоль
cat(assistant_content)
#############################################################################################################################
#--------------------------------------------------------Решения к задаче о бондах #1
#You are thinking about investing in a £1,000 face value bond which will mature in two years. 
#The bond has an 8% coupon and pays interest seminannually. The current yield to maturity on similar 
#bonds is 6%, and rates are not expected to change. What is the bond’s price today?
#1000*(0.08)/2 = 40 (Делим на 2 потому что семианнуал)
#6%/2 потому что семианнуал
#PV0 = 40/1.03 + 40/1.03^2 + 40/1.03^3 + 1040/1.03^4 = 1037.13
#PV0 = D/(1+R) если дивидендны выплачиваются через периоды добавляем размер дивиденда деленного на дискоунт рейт
#возведенный в степень равную периоду, в конце также добавляем стоимость всего бонда который будет выплачен в конце
#--------------------------------------------------------Решения к задаче о return #1
#Suppose you have a project that has a 70% chance of doubling your investment and a 30% chance of
#halving your investment in a year. What is the standard deviation of the rate of return of this investment?
#Solution
#Probability 30% ana 70%
#Rerurn     -50% and 100%
#Mean = 0.7*100 + 0.3*(-50) = 55%
#Variance = 0.7*(100-55)^2 + 0.3(-50-55)^2 = 1417,5 + 3307,5 = 4725
#SD = sqrt of 4725
#--------------------------------------------------------Решения к задаче о buy/sell #1
#We know that firm XYZ is very poorly run. On a scale of 1 (worst) to 10 (best), you would give it
#a score of 3. The market consensus evaluation is that the management score is only 2. Should you
#buy or sell the stock? Explain.
#Solution
#Buy. In your view, the firm is not as bad as everyone else believes it to be.
#Therefore, you view the firm as undervalued by the market. You are less pessimistic about the
#firm’s prospects than the beliefs built into the stock price.
#--------------------------------------------------------Решения к задаче о intrinsic value #1
#Deployment Specialists (DS) pays a current (annual) dividend of $1.00. DS dividend is expected to
#grow at 20% for 2 years and then at 4% thereafter. The required return for DS is 8.5%. 1. What is 
#the intrinsic value of DC stock?
#E[D1] = D0(1+r)= D0(1 + 0.2) =1$*(1+0.2)= 1.2
#E[D2] = D1(1 + 0.2) = 1.2 · 1.2 = 1.44
#E[D3] = D2(1.04) =1.44*1.04 = 1.4976
#P2 = E[D3]/(r-g) = 1.4976/(0.085 - 0.04) = 33.28
#P0 = E[D1]/(1+r) + E[D2]+P2/(1+r)^2 = 1.2/(1+0.085) + 1.44+33.28/(1+0.085)^2 = 30.6
#--------------------------------------------------------Portfolio
#You are constructing a portfolio. Among your choices are two risky assets, A and B: You are given
#the following estimates:
#----------A--------B
#Variance  0.25     0.49
#E[r]      0.10     0.16 
#The correlation of rA with rB is 0.7.
#Borrowing takes place at a risk-free interest rate of 5%. Suppose you purchase $1,000 of security A,
#purchase $500 of security B, and borrow $500. If these transactions constitute you entire portfolio,
#what are the portfolio weights for each component of the portfolio? 2. Compute expected return of this portfolio.
#3. Compute the variance of this portfolio.
#Solution
#$1000 invested in A
#$500 invested in B
#-$500 borrowed
#your equity in the portfolio $1000
#1. wa=1, wb=0.5, wc=-0.5
#2. E(r) = 1*0.1 + 0.5*0.16 + (-0.5)*0.05=0.155=15.5%
#3. Variance = σ2(rp)=1^2*0.25+0.5^2*0.49+2*1*0.5*√0.25*√0.49·0.7=0.6175
#--------------------------------------------------------Market model
#For this question, assume the settings of the single-factor market model.
#1. You are given the following information about stocks 1 and 2:
#β1 = 0.85 β2 = 1.30 The variance of the market factor is 0.09.
#(a) What is the covariance between stocks 1 and 2?
#Solution cov(r1, r2) = β1β2σm2 = 0.85 × 1.30 × 0.09 = 0.099
#Suppose you had estimated the following relationship for firm J’s return as a function of the return on a market factor
#rj = 0.03+1.3rm +εj
#(a) If the return on the market factor should fall by two percentage points, what is the
#expected change in firm J’s return?
#(b) What might account for J’s actual return being different from that expected return?
#Solution
#(a) All of the change in rj comes from the last term on the right 1.3rm, (change in rj) = 1.3 (change in rm) = 1.3(−0.2) = −0.026.
#Stock J would be expected to fall by 2.6 percentage points.
#(b) There may well be some company-specificeventsoccurringwhichcaseJ’sreturnstobesomething different than would be expected by looking at the first two terms of the equation alone and the market’s return for the period.
#--------------------------------------------------------Which bond to buy
#Bond Characteristics
#Bond ----------A--------------Bond B
#Coupons        3years          3years
#Coupon rate    10%             6%
#Yield to mat   10.65%          10.75%
#Price          98.4            88.34
#Spot interest rates
#---------------Term-----------Spot Rates
#               1             5%
#               2             8%
#               3             11%
#Solution
#PVA = 10/1.05 + 10/1.08^2 + 110/1.11^3 = 98.53
#PVB = 6/1.05 + 6/1.08^2 + 106/1.11^3 = 88.36
#Bond A sells by more attractive price, recomendations is bond A
#--------------------------------------------------------Sharp, CAPM, Jensen ratio
#SB = (0.1 − 0.04)/0.6 = 0.1. This is less than the Sharpe Index value for portfolios
#plotted on the capital market line (the straight line through m and A). Thus, portfolio B
#would be associated with under-performance.
#SA = (0.10−0.04)/0.4 = 0.15. Note that you can get this result even though you have not been 
#given the r value for A, since the Sharpe index will be the same along the straight line through m and A.
#Portfolio A would be one having neutral performance — no better and no worse than what could be
#achieved with a combination of the market and the riskless asset.
#From the information in the diagram, we can determine that any security having a beta of 0.5 ought
#to have an equilibrium average return of 7% under the CAPM. rf +β(rm −rf)=4%+0.5(10%−4%)=4%+0.5×6%=7%
#The Jensen index is the difference between the actual return and the CAPM predicted return
#J = 0.075 − 0.07 = 0.5% Since the Jensen index (alpha) is positive, you want to invest in this stock.
#----------------------------------------------------------Dividend growth model
#Daltrey Ltd earned AS$ 10 million for the fiscal year ending yesterday. The firm also paid out 20%
#of its earnings as dividends yesterday. The firm will continue to pay out 20% of its earnings as
#annual, end-of-year dividends. The remaining 80% of earnings is retained by the company for use in
#projects. The company has 1.25 million shares outstanding. The current share price is AS$ 40.
#The historical return on equity (ROE) of 11% is expected to continue in the future. What is the 
#required rate of return on Daltrey equity?
#Solution
#P0=(E[X]/r) + kE[X](r*-r) / r(r*k)
#P0=E[X](1-k)/r-r*k = E[D1]/r-r*k
#g = r*k
#ROE = 11%, k = 80%, g =0.11*0.8 = 0.088%
#P0 = E[D1]/r-g = D0(1+g)/r-g
#D0 = 2mil / 1.25mil = 1.6
#P0 = 1.6(1+0.088)/r-0.088 => 40 = 1.6(1+0.088)/r-0.088 => 40(r − 0.088) = 1.7408 =>
#r = 1.7408/40 + 0.088 = 0.13152 = 13.15% 40
#------------------------------------------Alpha
#Charlie Smith holds two portfolios, Portfolio X and Portfolio Y. They are both liquid, well-
#diversified portfolios with approximately equal market values. He expects Portfolio X to return
#13% and Portfolio Y to return 14% over the upcoming year. Because of an unexpected need for
#cash, Smith is forced to sell at least one of the portfolios. He uses the security market line to
#determine whether his portfolios are undervalued or overvalued. Portfolio X's beta is 0.9 and
#Portfolio Y's beta is 1.1. The expected return on the market is 12% and the risk-free rate is 5%.
#Smith should sell: Portfolio X’s required return is 0.05 + 0.9 × (0.12 − 0.05) = 11.3%. It is expected (actual) to return 13%.
#The portfolio has an expected excess return of 1.7% Alfa=13-11,3=1,7% undervalue
#Portfolio Y’s required return is 0.05 + 1.1 × (0.12 − 0.05) = 12.7%. It is expected to return 14%. The
#portfolio has an expected excess return of 1.3%. Undervalue
#Since both portfolios are undervalued, the investor should sell the portfolio that offers less excess return.
#Sell Portfolio Y because its excess return is less than that of Portfolio X. 

#---------------------------------------------SHORT ANSWERS-------------------
##1In the Treynor-Black model of active portfolio allocation there are two key properties of
#a stock which determines the active weight. Which are they? For each of them: If it was to increase,
#what would happen to the active weight?
#Answer - In the Treynor-Black model of active portfolio allocation the are two key properties of
#a stock which determines the active weight are αi and idiosyncratic variance/standard deviation, σ(ei).
##2 In the context of private equity investing: What is Dry Powder? What has been its development
#in the last couple of years?
#Answer - Dry Powder: Pledged, uninvested capital. It has increased significantly in the last couple of years
##3 In investments a key decision is the choice between asset classes. In this class we have spent most of
#the time on equity and fixed income investments. Which other asset classes have been discussed in this course?
#Answer - Asset classes: Derivatives, Currency (yield arbitrage), Private Equity, Real Estate, Crypto, Infrastructure.
#Suppose that two securities have a correlation coefficient of −1. What is the lowest possible standard deviation that could 
#be achieved by constructing a portfolio of these two securities?
#Answer - zero
##4 What criterion must a portfolio meet to be in the minimum variance set? Contrast the minimum variance
#set with the efficient set.
#Answer - The portfolio must have the lowest possible variance for any portfolio having the same expected
#return. The efficient set is the subset of portfolios in the miniumum variance set with the highest expected
#return.
##5 What position has more downside exposure: a short position in a call or a short position in a put?
#That is, in the worst case, in which of these two positions would your losses be greater?
#Answer - The loss for a put is bounded by the exercise price, while a call has unlimited losses.
##6 In a period of inflation, is the real rate of interest or the nominal rate of interest higher? Why?
#Answer - Nominal rate is higher, since nominal rate ≈ real rate + inflation
##7 What problems would there be in trying to derive information about the term structure of
#interest rates from data on corporate bond yields?
#Answer First, corporate bonds will have various degrees of default risk. In estimating a yield curve, we want
#to look at differences in yield that occur solely due to different maturity. In addition, many corporate
#bonds are callable, thereby having maturities which are ambigous.
##8 What is immunization intended to accomplish?
#Answer - Immunization is intended to protect one from the threat of interest rate changes. The investor 
#(a pension fund for example) is seeking assurance that it will be able to meet its obligations no matter
#what happen to interest rates.

##############################################Formula section##################
#--------------------------------------------Performance evaluation
#inflation           Ct = Xt(1+i)^t
#                    r=(1+Rn)/(1+i) - 1
#                    Rn = (1+r)(1+i) - 1
#                    Xt - realcash flow, Ct - nominal cash flow, i - inflation, r-real int rate, Rn - nominal int rate
#Present Value       PV = SUM(Ct/(1+r)^t)
#Sharp ratio         Si = Ri-Rf/SDi
#Treynor Ratio       Ti = Ri-Rf/Bi
#Jensens alpha       Alphai = Ri- (Rf+Bi(Rm-Rf))=Ri-Rf-Bi(Rm-Rf)
#Jensen rario        J = Ri - CAPM (это то же самое что и формула сверху)
#Alha general        Alphai = Ri - "Required Return"
#Information ratio   IR = Rp-Rb/SDx(Rp-Rb)
#Market model        Ri = Alphai + Bi*Rm +ei (ei=0)
#M2                  M2 = Rf + Excess Return
#M2                  M2 = Rf + (Rp-Rf/SDp)xSDb (SDb - benchmark SD, SDp-portfolio SD)
#CAPM                E(R) = Rf + B(Rm - Rf)
#Расчет бета(beta)   Bi = cov(ri,rm)/var(rm)
#CAPM portfolio return - найти Ri для каждого ассета, потом умножить на веса и просумировать
#тоже касается беты Bp = SUM(Wi*Bi)
#Covarince          Cov = B1*B2*Var
#Variance           Var = SUM(pi*(xi-m)^2)
#                         pi - вероятность исхода
#                         xi - значение исхода
#                         m -  любое среднее значение требуемое в задаче, например E(R)
#Variance2asstport  SDp^2=w1^2*sd1^2 + w2^2*SD2^2 + 2W1*W2*SD1*SD2*cov
#                         SD1^2 - дисперсия актива 1, SD2^2 - дисперсия актива 2 
#                         COV - ковариация (cov)
#Expected Div       E(D1)=D0*(1+g), E(D2)=D1*(1+g)....
#Price last year    P1 = E(D2)/r-g
#Price today        P0 = E(D1)/1+r + E(D2)/(1+r)^2 +P1/(1+r)^2 #аналогично расчитываем для любого количества 
#                   лет ориентируясь на последние доступные данные о дивидендах и grow rate
#Expected return    Er = E[D1]/P0 + E[P1] - P0/P0
#Price today        P0 = E[D1]+E[P1]/1+r
#Price today DDM    P0 = E[Dt]/(1+r)^t считаем цену для каждого периода и сумируем
#Price today DDM D1 P0 = E[D1]/r-g
#Отсюда можно найти r=(E[D1]/P0) + g
#Можно найти Dt как Dt = Xt - It (Xt - earnings, I - investments)
#Также можно найти  P0 = SUM (E[Xt] - E[It])/(1+r)^t
#Assets in place    AIP = E[X]/r это стоиомость активов вложенных в компанию
#PVGO = (r* - r)kE[X]/r(r-r*k) k-коефициент удержания капитала, r* - reinvestment return
#DDMG or GSVM       P0 = E[X]/r + kE[X](r*-r)/r(r-r*k)
#Или же так         P0 = E[X](1-k)/r-r*k = E[D1]/r-r*k
#                   в качестве r* можно использовать значение ROE
#Wi weight of asset Wj = Market Value of asset j / Market Value of all asset
#Er of portfolio    Er=SUM(Wj*E[rj])
#Дисперсия          SDp^2 = (rp - E[rp])^2 где rp - случайная доходность портфеля
#Пирсон коеф корел  pij = COV(I,J)/SDi*SDj
#Minimiz variance   W1 = SD2^2 - SD1*SD2*p12 / SD1^2+SD2^2 - 2SD1*SD2*p12 (p12-Пирсон коеф between 2 assets)
#Factor model       E[Rit] = rft +Bi(Rmt-Rft)+bsmb+bhml....
#                   Rit - return for asset
#                   Rft - risk free rate
#                   Rmt - market rate
#                   SMB, HML, CMA - models



