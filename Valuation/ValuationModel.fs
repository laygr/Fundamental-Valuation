namespace CompanyValuation
module ValuationModel =
    open DiffSharp.AD.Float64
    type BalanceSheet = {
        ``cash and equivalents`` : D
        ``short term investments`` : D
        ``total current assets`` : D
        ``total current liabilities`` : D
        ``current portion of long term debt`` : D
        ``current portion of capital leases`` : D
    } with
        member this.``total cash and ST Investments`` =
            this.``cash and equivalents``
            + this.``short term investments``
        member this.NetWorkingCapital =
            - this.``total cash and ST Investments``
            + this.``total current assets``
            - this.``total current liabilities``
            + this.``current portion of long term debt``
            + this.``current portion of capital leases``
    type IncomeStatement = {
        Revenue : D
        ``cost of goods sold`` : D
        ``selling general and admin exp`` : D
        ``other operating expense (income)`` : D
        ``Interest Expense`` : D
    } with member this.Ebit =
            this.Revenue
            - this.``cost of goods sold``
            - this.``selling general and admin exp``
            - this.``other operating expense (income)``
    type CashFlow = {
        ``Depreciation and Amort, Total`` : D
        ``Stock-Based Compensation`` : D
        ``Capital Expenditure`` : D
        ``Sale (Purchase) of Intangible assets`` : D
    }
    type ModelVariables = {
        ``cash and equivalents growth`` : D
        ``short term investments growth`` : D
        ``total current assets growth`` : D
        ``total current liabilities growth`` : D
        ``current portion of long term debt growth`` : D
        ``current portion of capital leases growth`` : D
        ``revenue growth``: D
        ``cost of goods sold growth`` : D
        ``selling general and admin exp growth`` : D
        ``other operating expense (income) growth`` : D
        ``Interest Expense growth`` : D
        ``Depreciation and Amort, Total growth`` : D
        ``Stock-Based Compensation growth`` : D
        ``Capital Expenditure growth`` : D
        ``Sale (Purchase) of Intangible assets growth`` : D
    } with static member public Create
            ``cash and equivalents growth``
            ``short term investments growth``
            ``total current assets growth``
            ``total current liabilities growth``
            ``current portion of long term debt growth``
            ``current portion of capital leases growth``
            ``revenue growth``
            ``cost of goods sold growth``
            ``selling general and admin exp growth``
            ``other operating expense (income) growth``
            ``interest Expense growth``
            ``depreciation and Amort, Total growth``
            ``stock-Based Compensation growth``
            ``capital Expenditure growth``
            ``sale (Purchase) of Intangible assets growth``
            =
            {
                ``cash and equivalents growth`` = ``cash and equivalents growth``
                ``short term investments growth`` = ``short term investments growth``
                ``total current assets growth`` = ``total current assets growth``
                ``total current liabilities growth`` = ``total current liabilities growth``
                ``current portion of long term debt growth`` = ``current portion of long term debt growth``
                ``current portion of capital leases growth`` = ``current portion of capital leases growth``
                ``revenue growth`` = ``revenue growth``
                ``cost of goods sold growth`` = ``cost of goods sold growth``
                ``selling general and admin exp growth`` = ``selling general and admin exp growth``
                ``other operating expense (income) growth`` = ``other operating expense (income) growth``
                ``Interest Expense growth`` = ``interest Expense growth``
                ``Depreciation and Amort, Total growth`` = ``depreciation and Amort, Total growth``
                ``Stock-Based Compensation growth`` = ``stock-Based Compensation growth``
                ``Capital Expenditure growth`` = ``capital Expenditure growth``
                ``Sale (Purchase) of Intangible assets growth`` = ``sale (Purchase) of Intangible assets growth``
            }
            static member public CreateWithFloats
                ``cash and equivalents growth``
                ``short term investments growth``
                ``total current assets growth``
                ``total current liabilities growth``
                ``current portion of long term debt growth``
                ``current portion of capital leases growth``
                ``revenue growth``
                ``cost of goods sold growth``
                ``selling general and admin exp growth``
                ``other operating expense (income) growth``
                ``interest Expense growth``
                ``depreciation and Amort, Total growth``
                ``stock-Based Compensation growth``
                ``capital Expenditure growth``
                ``sale (Purchase) of Intangible assets growth``
            =
            {
                ``cash and equivalents growth`` = D ``cash and equivalents growth``
                ``short term investments growth`` = D ``short term investments growth``
                ``total current assets growth`` = D ``total current assets growth``
                ``total current liabilities growth`` = D ``total current liabilities growth``
                ``current portion of long term debt growth`` = D ``current portion of long term debt growth``
                ``current portion of capital leases growth`` = D ``current portion of capital leases growth``
                ``revenue growth`` = D ``revenue growth``
                ``cost of goods sold growth`` = D ``cost of goods sold growth``
                ``selling general and admin exp growth`` = D ``selling general and admin exp growth``
                ``other operating expense (income) growth`` = D ``other operating expense (income) growth``
                ``Interest Expense growth`` = D ``interest Expense growth``
                ``Depreciation and Amort, Total growth`` = D ``depreciation and Amort, Total growth``
                ``Stock-Based Compensation growth`` = D ``stock-Based Compensation growth``
                ``Capital Expenditure growth`` = D ``capital Expenditure growth``
                ``Sale (Purchase) of Intangible assets growth`` = D ``sale (Purchase) of Intangible assets growth``
            }
            member this.AsList =
                [
                    this.``cash and equivalents growth``
                    this.``short term investments growth``
                    this.``total current assets growth``
                    this.``total current liabilities growth``
                    this.``current portion of long term debt growth``
                    this.``current portion of capital leases growth``
                    this.``revenue growth``
                    this.``cost of goods sold growth``
                    this.``selling general and admin exp growth``
                    this.``other operating expense (income) growth``
                    this.``Interest Expense growth``
                    this.``Depreciation and Amort, Total growth``
                    this.``Stock-Based Compensation growth``
                    this.``Capital Expenditure growth``
                    this.``Sale (Purchase) of Intangible assets growth``
                ]
    let growBy (value:D) (growth:D) = value * (1. + growth)
    let next balanceSheet incomeStatement cashFlow modelVariables =
        let nextBalanceSheet = {
            ``cash and equivalents`` = growBy balanceSheet.``cash and equivalents`` modelVariables.``cash and equivalents growth``
            ``short term investments`` = growBy balanceSheet.``short term investments`` modelVariables.``short term investments growth``
            ``total current assets`` = growBy balanceSheet.``total current assets`` modelVariables.``total current assets growth``
            ``total current liabilities`` = growBy balanceSheet.``total current liabilities`` modelVariables.``total current liabilities growth``
            ``current portion of long term debt`` = growBy balanceSheet.``current portion of long term debt`` modelVariables.``current portion of long term debt growth``
            ``current portion of capital leases`` = growBy balanceSheet.``current portion of capital leases``modelVariables.``current portion of capital leases growth``
        }
        let nextIncomeStatement = {
            Revenue = growBy incomeStatement.Revenue modelVariables.``revenue growth``
            ``cost of goods sold`` = growBy incomeStatement.``cost of goods sold`` modelVariables.``cost of goods sold growth``
            ``selling general and admin exp`` = growBy incomeStatement.``selling general and admin exp`` modelVariables.``selling general and admin exp growth``
            ``other operating expense (income)`` = growBy incomeStatement.``other operating expense (income)`` modelVariables.``other operating expense (income) growth``
            ``Interest Expense`` = growBy incomeStatement.``Interest Expense`` modelVariables.``Interest Expense growth``
        }
        let nextCashFlow = {
            ``Depreciation and Amort, Total`` = growBy cashFlow.``Depreciation and Amort, Total`` modelVariables.``Depreciation and Amort, Total growth``
            ``Stock-Based Compensation`` = growBy cashFlow.``Stock-Based Compensation`` modelVariables.``Stock-Based Compensation growth``
            ``Capital Expenditure`` = growBy cashFlow.``Capital Expenditure`` modelVariables.``Capital Expenditure growth``
            ``Sale (Purchase) of Intangible assets`` = growBy cashFlow.``Sale (Purchase) of Intangible assets`` modelVariables.``Sale (Purchase) of Intangible assets growth``
        }
        nextBalanceSheet, nextIncomeStatement, nextCashFlow
    let freeCashFlow (balanceSheet:BalanceSheet) (incomeStatement:IncomeStatement) (cashFlow:CashFlow) (pastBalanceSheet:BalanceSheet) =
        let ebit = incomeStatement.Ebit
        let changeInNetWorkingCapital = balanceSheet.NetWorkingCapital - pastBalanceSheet.NetWorkingCapital
        ebit * (1. - 0.375)
        + incomeStatement.``Interest Expense`` * (1. - 0.375)
        + cashFlow.``Depreciation and Amort, Total``
        + cashFlow.``Capital Expenditure``
        + cashFlow.``Sale (Purchase) of Intangible assets``
        + cashFlow.``Stock-Based Compensation``
        - changeInNetWorkingCapital

    let terminalValue (fcf5:D) (wacc:D) (growth:D) =
        fcf5 * (1.+growth)/(wacc-growth)

    let enterpriseValue
        (fcf1:D) (fcf2:D) (fcf3:D) (fcf4:D) (fcf5:D) (``year-5 terminal value``:D) (wacc:D) =
        fcf1/(1.+wacc)**0.5
        + fcf2/(1.+wacc)**1.5
        + fcf3/(1.+wacc)**2.5
        + fcf4/(1.+wacc)**3.5
        + fcf5/(1.+wacc)**4.5
        + ``year-5 terminal value``/(1.+wacc)**4.5

    let equityValue enterpriseValuePV addInInitialCashAndMktSecurities =
        enterpriseValuePV + addInInitialCashAndMktSecurities

    let stockValue equityValue sharesOutstanding =
        equityValue / sharesOutstanding

    let modelValuation
        balanceSheet incomeStatement cashFlow sharesOutstanding
        wacc growth modelVariables1 modelVariables2 modelVariables3 modelVariables4 modelVariables5
         =
        let balanceSheet1, incomeStatement1, cashFlow1 = next balanceSheet incomeStatement cashFlow modelVariables1
        let balanceSheet2, incomeStatement2, cashFlow2 = next balanceSheet1 incomeStatement1 cashFlow1 modelVariables2
        let balanceSheet3, incomeStatement3, cashFlow3 = next balanceSheet2 incomeStatement2 cashFlow2 modelVariables3
        let balanceSheet4, incomeStatement4, cashFlow4 = next balanceSheet3 incomeStatement3 cashFlow3 modelVariables4
        let balanceSheet5, incomeStatement5, cashFlow5 = next balanceSheet4 incomeStatement4 cashFlow4 modelVariables5

        let fcf1 = freeCashFlow balanceSheet1 incomeStatement1 cashFlow1 balanceSheet
        let fcf2 = freeCashFlow balanceSheet2 incomeStatement2 cashFlow2 balanceSheet1
        let fcf3 = freeCashFlow balanceSheet3 incomeStatement3 cashFlow3 balanceSheet2
        let fcf4 = freeCashFlow balanceSheet4 incomeStatement4 cashFlow4 balanceSheet3
        let fcf5 = freeCashFlow balanceSheet5 incomeStatement5 cashFlow5 balanceSheet4

        let terminalValue = terminalValue fcf5 wacc growth
        let enterpriseValue = enterpriseValue fcf1 fcf2 fcf3 fcf4 fcf5 terminalValue wacc
        let equityValue = equityValue enterpriseValue balanceSheet.``cash and equivalents``
        stockValue equityValue sharesOutstanding
    
    let variablesToVector wacc growth
        (modelVariables1:ModelVariables) (modelVariables2:ModelVariables) (modelVariables3:ModelVariables)
        (modelVariables4:ModelVariables) (modelVariables5:ModelVariables) =
        toDV (List.concat [[wacc; growth]; modelVariables1.AsList; modelVariables2.AsList; modelVariables3.AsList; modelVariables4.AsList; modelVariables5.AsList; ])

    let modelValuation' balanceSheet incomeStatement cashFlow sharesOutstanding (x:DV) =
        let modelVariables1 = ModelVariables.Create x.[2] x.[3] x.[4] x.[5] x.[6] x.[7] x.[8] x.[9] x.[10] x.[11] x.[12] x.[13] x.[14] x.[15] x.[16]
        let modelVariables2 = ModelVariables.Create x.[17] x.[18] x.[19] x.[20] x.[21] x.[22] x.[23] x.[24] x.[25] x.[26] x.[27] x.[28] x.[29] x.[30] x.[31]
        let modelVariables3 = ModelVariables.Create x.[32] x.[33] x.[34] x.[35] x.[36] x.[37] x.[38] x.[39] x.[40] x.[41] x.[42] x.[43] x.[44] x.[45] x.[46]
        let modelVariables4 = ModelVariables.Create x.[47] x.[48] x.[49] x.[50] x.[51] x.[52] x.[53] x.[54] x.[55] x.[56] x.[57] x.[58] x.[59] x.[60] x.[61]
        let modelVariables5 = ModelVariables.Create x.[62] x.[63] x.[64] x.[65] x.[66] x.[67] x.[68] x.[69] x.[70] x.[71] x.[72] x.[73] x.[74] x.[75] x.[76]
        modelValuation balanceSheet incomeStatement cashFlow sharesOutstanding x.[0] x.[1] modelVariables1 modelVariables2 modelVariables3 modelVariables4 modelVariables5