namespace CompanyValuation
module CompanyModel =
    type BalanceSheet = {
        ``cash and equivalents`` : float
        ``short term investments`` : float
        ``total current assets`` : float
        ``total current liabilities`` : float
        ``current portion of long term debt`` : float
        ``current portion of capital leases`` : float
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
        Revenue : float
        ``cost of goods sold`` : float
        ``selling general and admin exp`` : float
        ``other operating expense (income)`` : float
        ``Interest Expense`` : float
    } with member this.Ebit =
            this.Revenue
            - this.``cost of goods sold``
            - this.``selling general and admin exp``
            - this.``other operating expense (income)``
    type CashFlow = {
        ``Depreciation and Amort, Total`` : float
        ``Stock-Based Compensation`` : float
        ``Capital Expenditure`` : float
        ``Sale (Purchase) of Intangible assets`` : float
    }
    let freeCashFlow (balanceSheet:BalanceSheet) (incomeStatement:IncomeStatement) (cashFlow:CashFlow) (pastBalanceSheet:BalanceSheet) =
        let ebit = incomeStatement.Ebit
        let changeInNetWorkingCapital = balanceSheet.NetWorkingCapital - pastBalanceSheet.NetWorkingCapital
        
        ebit * (1. - 0.375)
        + incomeStatement.``Interest Expense`` * (1. - 0.375)
        + cashFlow.``Depreciation and Amort, Total``
        + cashFlow.``Capital Expenditure``
        + cashFlow.``Sale (Purchase) of Intangible assets``
        - changeInNetWorkingCapital