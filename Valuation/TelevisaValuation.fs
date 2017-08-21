namespace CompanyValuation
module TelevisaValuation =
    open ValuationModel
    open DiffSharp.AD.Float64

    let balanceSheet = {
        ``cash and equivalents`` = D  47546.1
        ``short term investments`` = D 5498.2
        ``total current assets`` = D 95761.5
        ``total current liabilities`` = D 57426.1
        ``current portion of long term debt`` = D 3880.6
        ``current portion of capital leases`` = D 575.6
    }
    let incomeStatement = {
        Revenue = D 96287.4
        ``cost of goods sold`` = D 52377.8
        ``selling general and admin exp`` = D 24174.1
        ``other operating expense (income)`` = D 1407.2
        ``Interest Expense`` = D -8644.7
    }
    let cashFlow = {
        ``Depreciation and Amort, Total`` = D 17332.5
        ``Stock-Based Compensation`` = D 1410.5
        ``Capital Expenditure`` = D -27941.6
        ``Sale (Purchase) of Intangible assets`` = D -2472.1
    }
    let sharesOutstanding = D 2478.7

    let wacc = D 0.1241
    let growth = D 0.025

    let modelVariables1 =
        {
            ``cash and equivalents growth``                 = D -0.2169327 //
            ``short term investments growth``               = D 0.02667661 //
            ``total current assets growth``                 = D -0.218644836 //
            ``total current liabilities growth``            = D 0.168644152 //
            ``current portion of long term debt growth``    = D 2.41071945 //
            ``current portion of capital leases growth``    = D 0.5699060441 //
            ``revenue growth``                              = D -0.033792 //
            ``cost of goods sold growth``                   = D 0.001409 //
            ``selling general and admin exp growth``        = D -0.015706895 //
            ``other operating expense (income) growth``     = D -0.270905627 //
            ``Interest Expense growth``                     = D 0.06742048938 //
            ``Depreciation and Amort, Total growth``        = D 0.04956842 //
            ``Stock-Based Compensation growth``             = D  0.03610442 //
            ``Capital Expenditure growth``                  = D -0.172216337 //
            ``Sale (Purchase) of Intangible assets growth`` = D -0.117915942 //
        }
    let modelVariables2 =
        {
            ``cash and equivalents growth``                 = D 0.1 //
            ``short term investments growth``               = D -0.01103698 //
            ``total current assets growth``                 = D 0.06556662 //
            ``total current liabilities growth``            = D 0.218495323 //
            ``current portion of long term debt growth``    = D -0.15 //
            ``current portion of capital leases growth``    = D 0.2354213916 //
            ``revenue growth``                              = D 0.05 //
            ``cost of goods sold growth``                   = D 0.03 //
            ``selling general and admin exp growth``        = D 0.1 //
            ``other operating expense (income) growth``     = D -0.3 //
            ``Interest Expense growth``                     = D -0.001 //
            ``Depreciation and Amort, Total growth``        = D 0.058180733 //
            ``Stock-Based Compensation growth``             = D 0.0 //
            ``Capital Expenditure growth``                  = D -0.05 //
            ``Sale (Purchase) of Intangible assets growth`` = D 0.0 //
        }
    let modelVariables3 =
        {
            ``cash and equivalents growth``                 = D 0.1 //
            ``short term investments growth``               = D -0.01103698 //
            ``total current assets growth``                 = D 0.108669451 //
            ``total current liabilities growth``            = D 0.130251151 //
            ``current portion of long term debt growth``    = D -0.15 //
            ``current portion of capital leases growth``    = D 0.2354213916 //
            ``revenue growth``                              = D 0.05 //
            ``cost of goods sold growth``                   = D 0.03 //
            ``selling general and admin exp growth``        = D 0.1 //
            ``other operating expense (income) growth``     = D -0.3 //
            ``Interest Expense growth``                     = D -0.001 //
            ``Depreciation and Amort, Total growth``        = D 0.058180733 //
            ``Stock-Based Compensation growth``             = D 0.0 //
            ``Capital Expenditure growth``                  = D -0.05 //
            ``Sale (Purchase) of Intangible assets growth`` = D 0.0 //
        }
    let modelVariables4 =
        {
            ``cash and equivalents growth``                 = D 0.1 //
            ``short term investments growth``               = D -0.01103698 //
            ``total current assets growth``                 = D 0.119739717 //
            ``total current liabilities growth``            = D 0.145429116 //
            ``current portion of long term debt growth``    = D -0.15 //
            ``current portion of capital leases growth``    = D 0.2354213916 //
            ``revenue growth``                              = D 0.05 //
            ``cost of goods sold growth``                   = D 0.03 //
            ``selling general and admin exp growth``        = D 0.1 //
            ``other operating expense (income) growth``     = D -0.3 //
            ``Interest Expense growth``                     = D -0.001 //
            ``Depreciation and Amort, Total growth``        = D 0.058180733 //
            ``Stock-Based Compensation growth``             = D 0.0 //
            ``Capital Expenditure growth``                  = D -0.05 //
            ``Sale (Purchase) of Intangible assets growth`` = D 0.0 //
        }
    let modelVariables5 =
        {
            ``cash and equivalents growth``                 = D 0.1 //
            ``short term investments growth``               = D -0.01103698 //
            ``total current assets growth``                 = D 0.132942561 //
            ``total current liabilities growth``            = D 0.157438857 //
            ``current portion of long term debt growth``    = D -0.15 //
            ``current portion of capital leases growth``    = D 0.2354213916 //
            ``revenue growth``                              = D 0.05 //
            ``cost of goods sold growth``                   = D 0.03 //
            ``selling general and admin exp growth``        = D 0.1 //
            ``other operating expense (income) growth``     = D -0.3 //
            ``Interest Expense growth``                     = D -0.001 //
            ``Depreciation and Amort, Total growth``        = D 0.058180733 //
            ``Stock-Based Compensation growth``             = D 0.0 //
            ``Capital Expenditure growth``                  = D -0.05 //
            ``Sale (Purchase) of Intangible assets growth`` = D 0.0 //
        }

    let valuate =
        let modelValuation = modelValuation' balanceSheet incomeStatement cashFlow sharesOutstanding
        let values = ValuationModel.variablesToVector wacc growth modelVariables1 modelVariables2 modelVariables3 modelVariables4 modelVariables5
        let stockValue = modelValuation values
        let gradient = values |> grad modelValuation // se computa el gradiente del modelo
        let elasticities =
            Seq.zip (DV.toArray gradient) (DV.toArray values)
            |> Seq.map (fun (g, v) -> (stockValue / v) * g) // se computan las elasticidades
            |> Seq.toArray
        stockValue, gradient, elasticities

    