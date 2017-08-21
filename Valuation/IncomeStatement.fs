module IncomeStatement
    type Revenue = {
        ``Net Sales`` : float
    } with member this.Value = this.``Net Sales``
    type ``Other Revenue`` = {
        Other : float
    } with member this.Value = this.Other
    type ``Cost Of Goods Sold`` = {
        ``Cost of Sales`` : float
    } with member this.Value = this.``Cost of Sales``
    type ``Selling General And Admin Exp`` = {
        ``SBC (Gen And Admin Exp)`` : float
        ``Selling Expenses`` : float
        ``General and Administrative Expenses`` : float
    } with member this.Value =
            this.``Selling Expenses``
            + this.``SBC (Gen And Admin Exp)``
            + this.``General and Administrative Expenses``
    type ``R And D Exp`` = {
        Other : float
    } with member this.Value = this.Other
    type ``Depreciation And Amort`` = {
        Other : float
    } with member this.Value = this.Other
    type ``Other Operating Expense (Income)`` = {
        ``Other Income/expense-net`` : float
        ``Non-Operating (Income) Expenses`` : float
        ``Gain (loss) On Sale of Assets`` : float
        ``Restructuring Charges`` : float
        ``Impairment of Goodwill`` : float
        ``Asset Writedown`` : float
    } with member this.Value =
            this.``Other Income/expense-net``
            - this.``Gain (loss) On Sale of Assets``
            - this.``Non-Operating (Income) Expenses``
            - this.``Restructuring Charges``
            - this.``Impairment of Goodwill``
            - this.``Asset Writedown``
    type ``Interest Expense`` = {
        ``Interest Expense`` : float
    } with member this.Value = this.``Interest Expense``
    type ``Interest and Invest Income`` = {
        ``Interest and Investment Income`` : float
    } with member this.Value = this.``Interest and Investment Income``
    type ``Income (Loss) from Affiliates`` = {
        ``Participation in Results of Subsidiary N...`` : float
        ``Share of Income of Associates and Joint ...`` : float
        ``Share of income (loss) of Assciates and ...`` : float
    } with member this.Value =
            this.``Participation in Results of Subsidiary N...``
            + this.``Share of Income of Associates and Joint ...``
            + this.``Share of income (loss) of Assciates and ...``
    type ``Currency Exchange Gains (Loss)`` = {
        ``Currency Translation Gain (Loss)`` : float
    } with member this.Value = this.``Currency Translation Gain (Loss)``
    type ``Other Non-Operating Inc (Exp)`` = {
        Other : float
    } with member this.Value = this.Other
    type ``Restructuring Charges`` = {
        ``Restructuring Charges`` : float
    } with member this.Value = this.``Restructuring Charges``
    type ``Impairment of Goodwill`` = {
        ``Impairment of GoodWill`` : float
    } with member this.Value = this.``Impairment of GoodWill``
    type ``Gain (Loss) On Sale of Invest`` = {
        ``Gain (Loss) On Sale of Invest`` : float
    } with member this.Value = this.``Gain (Loss) On Sale of Invest``
    type ``Gain (Loss) On Sale Of Assets`` = {
        ``Gain (Loss) On Sale Of Assets`` : float
    } with member this.Value = this.``Gain (Loss) On Sale Of Assets``
    type ``Asset Writedown`` = {
        ``Asset Writedown`` : float
    } with member this.Value = this.``Asset Writedown``
    type ``Other Unusual Items`` = {
        ``Non-Operating (Income) Expenses`` : float
    } with member this.Value = this.``Non-Operating (Income) Expenses``
    type ``Income Tax Expense`` = {
        ``Provision for Income Tax`` : float
    } with member this.Value = this.``Provision for Income Tax``
    type ``Earnings of Discounted Ops`` = {
        Other : float
    } with member this.Value = this.Other
    type ``Extraord Item And Account Change`` = {
        Other : float
    } with member this.Value = this.Other
    type ``Minority Int in Earnings`` = {
        ``Minority Interest After-tax`` : float
    } with member this.Value = this.``Minority Interest After-tax``
    type ``Pref Dividends and Other Adj`` = {
        Other : float
    } with member this.Value = this.Other

    type IncomeStatement = {
        Revenue : Revenue
        ``Other Revenue`` : ``Other Revenue``
        ``Cost Of Goods Sold``: ``Cost Of Goods Sold``
        ``Selling General And Admin Exp``: ``Selling General And Admin Exp``
        ``R And D Exp`` : ``R And D Exp``
        ``Depreciation And Amort`` : ``Depreciation And Amort``
        ``Other Operating Expense (Income)`` : ``Other Operating Expense (Income)``
        ``Interest Expense`` : ``Interest Expense``
        ``Interest and Invest Income`` : ``Interest and Invest Income``
        ``Income (Loss) from Affiliates`` : ``Income (Loss) from Affiliates``
        ``Currency Exchange Gains (Loss)`` : ``Currency Exchange Gains (Loss)``
        ``Other Non-Operating Inc (Exp)`` : ``Other Non-Operating Inc (Exp)``
        ``Restructuring Charges`` : ``Restructuring Charges``
        ``Impairment of Goodwill`` : ``Impairment of Goodwill``
        ``Gain (Loss) On Sale Of Invest`` : ``Gain (Loss) On Sale of Invest``
        ``Gain (Loss) On Sale of Assets`` : ``Gain (Loss) On Sale Of Assets``
        ``Asset Writedown`` : ``Asset Writedown``
        ``Other Unusual Items`` : ``Other Unusual Items``
        ``Income Tax Expense`` : ``Income Tax Expense``
        ``Earnings of Discontinued Ops`` : ``Earnings of Discounted Ops``
        ``Extraord Item And Account Change`` : ``Extraord Item And Account Change``
        ``Minority Int in Earnings`` : ``Minority Int in Earnings``
        ``Pref Dividends and Other Adj`` : ``Pref Dividends and Other Adj``
    } with
        member this.TotalRevenue =
            this.Revenue.Value + this.``Other Revenue``.Value
        member this.GrossProfit =
            this.TotalRevenue - this.``Cost Of Goods Sold``.Value
        member this.``Other Operating Exp, Total`` =
            this.``Selling General And Admin Exp``.Value
            + this.``Other Operating Expense (Income)``.Value
        member this.OperatingIncome =
            this.TotalRevenue
            - this.``Cost Of Goods Sold``.Value
            - this.``Other Operating Exp, Total``
        member this.NetInterestExpenses =
            this.``Interest Expense``.Value
            + this.``Interest and Invest Income``.Value
        member this.``Other Non Operating Expenses, Total`` =
            this.``Other Non-Operating Inc (Exp)``.Value
            + this.``Currency Exchange Gains (Loss)``.Value
            + 0.
        member this.``EBT Excl Unusual Items`` =
            this.TotalRevenue
            - this.``Cost Of Goods Sold``.Value
            - this.``Other Operating Exp, Total``
            + this.NetInterestExpenses
            + this.``Other Non Operating Expenses, Total``