namespace CompanyValuation

module Cholesky =
    open ValuationModel
    open DiffSharp.AD.Float64
    
    open MathNet.Numerics.LinearAlgebra
    open MathNet.Numerics.LinearAlgebra.Factorization

    open FSharp.Charting
    open MathNet.Numerics.Statistics
    
    let asArray listOfLists =
        let n = List.length listOfLists
        let arr = Array2D.zeroCreate n n
        
        Seq.iteri (fun row l ->
            Seq.iteri(fun column v ->
                    arr.[row, column] <- v
                 )l
           ) listOfLists
        arr

    let matrix (listOfLists) =
        let array = listOfLists |> asArray
        Matrix.Build.DenseOfArray(array)

    let valuesForSimulation
        waccDelta //
        ``total current liabilities growth 5 delta`` //
        ``total current assets growth 5 delta`` //
        ``total current assets growth 1 delta`` //
        ``capital Expenditure growth 1 delta`` //
        ``total current liabilities growth 2 delta`` //
        ``cash and equivalents growth 5 delta`` //
        ``growth delta`` //
        ``total current liabilities growth 1 delta`` //
        ``revenue growth 2 delta`` //
        ``revenue growth 3 delta`` //
        ``revenue growth 4 delta`` //
        ``revenue growth 1 delta`` //
        ``total current liabilities growth 4 delta`` //
        ``revenue growth 5 delta`` //
        =

        let wacc = D (0.1241 + waccDelta)
        let growth = D (0.025 + ``growth delta``)

        let modelVariables1 =
            {
                ``cash and equivalents growth``                 = D -0.2169327 //
                ``short term investments growth``               = D 0.02667661 //
                ``total current assets growth``                 = D (-0.218644836 + ``total current assets growth 1 delta``) //
                ``total current liabilities growth``            = D (0.168644152 + ``total current liabilities growth 1 delta``) //
                ``current portion of long term debt growth``    = D 2.41071945 //
                ``current portion of capital leases growth``    = D 0.5699060441 //
                ``revenue growth``                              = D (-0.033792 + ``revenue growth 1 delta``) //
                ``cost of goods sold growth``                   = D 0.001409 //
                ``selling general and admin exp growth``        = D -0.015706895 //
                ``other operating expense (income) growth``     = D -0.270905627 //
                ``Interest Expense growth``                     = D 0.06742048938 //
                ``Depreciation and Amort, Total growth``        = D 0.04956842 //
                ``Stock-Based Compensation growth``             = D  0.03610442 //
                ``Capital Expenditure growth``                  = D (-0.172216337 + ``capital Expenditure growth 1 delta``)//
                ``Sale (Purchase) of Intangible assets growth`` = D -0.117915942 //
            }
        let modelVariables2 =
            {
                ``cash and equivalents growth``                 = D 0.1 //
                ``short term investments growth``               = D -0.01103698 //
                ``total current assets growth``                 = D 0.06556662 //
                ``total current liabilities growth``            = D (0.218495323 + ``total current liabilities growth 2 delta``)//
                ``current portion of long term debt growth``    = D -0.15 //
                ``current portion of capital leases growth``    = D 0.2354213916 //
                ``revenue growth``                              = D (0.05 + ``revenue growth 2 delta``) //
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
                ``revenue growth``                              = D (0.05 + ``revenue growth 3 delta``)//
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
                ``total current liabilities growth``            = D (0.145429116 + ``total current liabilities growth 4 delta``) //
                ``current portion of long term debt growth``    = D -0.15 //
                ``current portion of capital leases growth``    = D 0.2354213916 //
                ``revenue growth``                              = D (0.05 + ``revenue growth 4 delta``) //
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
                ``cash and equivalents growth``                 = D (0.1 + ``cash and equivalents growth 5 delta``) //
                ``short term investments growth``               = D -0.01103698 //
                ``total current assets growth``                 = D (0.132942561 + ``total current assets growth 5 delta``)//
                ``total current liabilities growth``            = D (0.157438857 + ``total current liabilities growth 5 delta``) //
                ``current portion of long term debt growth``    = D -0.15 //
                ``current portion of capital leases growth``    = D 0.2354213916 //
                ``revenue growth``                              = D (0.05 + ``revenue growth 5 delta``) //
                ``cost of goods sold growth``                   = D 0.03 //
                ``selling general and admin exp growth``        = D 0.1 //
                ``other operating expense (income) growth``     = D -0.3 //
                ``Interest Expense growth``                     = D -0.001 //
                ``Depreciation and Amort, Total growth``        = D 0.058180733 //
                ``Stock-Based Compensation growth``             = D 0.0 //
                ``Capital Expenditure growth``                  = D -0.05 //
                ``Sale (Purchase) of Intangible assets growth`` = D 0.0 //
            }
        wacc, growth, modelVariables1, modelVariables2, modelVariables3, modelVariables4, modelVariables5

    let valuate wacc growth modelVariables1 modelVariables2 modelVariables3 modelVariables4 modelVariables5 =
        let f = modelValuation' TelevisaValuation.balanceSheet TelevisaValuation.incomeStatement TelevisaValuation.cashFlow TelevisaValuation.sharesOutstanding
        let values = ValuationModel.variablesToVector wacc growth modelVariables1 modelVariables2 modelVariables3 modelVariables4 modelVariables5
        let stockValue = f values
        stockValue
    
    let simulation () =
        (*
        let varcovar =
            [
                [0.018009012;-0.00364506;0.014851179;0.014851179;-0.007347967;-0.00364506;0.014252358;0.018009012;-0.00364506;-0.000434948;-0.000434948;-0.000434948;-0.000434948;-0.00364506;-0.000434948]
                [-0.00364506;0.002900835;-0.000268379;-0.000268379;-0.000661156;0.002900835;-0.003177794;-0.00364506;0.002900835;-0.000338085;-0.000338085;-0.000338085;-0.000338085;0.002900835;-0.000338085]
                [0.014851179;-0.000268379;0.033262036;0.033262036;-0.006797981;-0.000268379;0.054530078;0.014851179;-0.000268379;0.000237943;0.000237943;0.000237943;0.000237943;-0.000268379;0.000237943]
                [0.014851179;-0.000268379;0.033262036;0.033262036;-0.006797981;-0.000268379;0.054530078;0.014851179;-0.000268379;0.000237943;0.000237943;0.000237943;0.000237943;-0.000268379;0.000237943]
                [-0.007347967;-0.000661156;-0.006797981;-0.006797981;0.021113209;-0.000661156;0.01573657;-0.007347967;-0.000661156;-5.61003E-05;-5.61003E-05;-5.61003E-05;-5.61003E-05;-0.000661156;-5.61003E-05]
                [-0.00364506;0.002900835;-0.000268379;-0.000268379;-0.000661156;0.002900835;-0.003177794;-0.00364506;0.002900835;-0.000338085;-0.000338085;-0.000338085;-0.000338085;0.002900835;-0.000338085]
                [0.014252358;-0.003177794;0.054530078;0.054530078;0.01573657;-0.003177794;0.134448824;0.014252358;-0.003177794;0.00168961;0.00168961;0.00168961;0.00168961;-0.003177794;0.00168961]
                [0.018009012;-0.00364506;0.014851179;0.014851179;-0.007347967;-0.00364506;0.014252358;0.018009012;-0.00364506;-0.000434948;-0.000434948;-0.000434948;-0.000434948;-0.00364506;-0.000434948]
                [-0.00364506;0.002900835;-0.000268379;-0.000268379;-0.000661156;0.002900835;-0.003177794;-0.00364506;0.002900835;-0.000338085;-0.000338085;-0.000338085;-0.000338085;0.002900835;-0.000338085]
                [-0.000434948;-0.000338085;0.000237943;0.000237943;-5.61003E-05;-0.000338085;0.00168961;-0.000434948;-0.000338085;0.000207073;0.000207073;0.000207073;0.000207073;-0.000338085;0.000207073]
                [-0.000434948;-0.000338085;0.000237943;0.000237943;-5.61003E-05;-0.000338085;0.00168961;-0.000434948;-0.000338085;0.000207073;0.000207073;0.000207073;0.000207073;-0.000338085;0.000207073]
                [-0.000434948;-0.000338085;0.000237943;0.000237943;-5.61003E-05;-0.000338085;0.00168961;-0.000434948;-0.000338085;0.000207073;0.000207073;0.000207073;0.000207073;-0.000338085;0.000207073]
                [-0.000434948;-0.000338085;0.000237943;0.000237943;-5.61003E-05;-0.000338085;0.00168961;-0.000434948;-0.000338085;0.000207073;0.000207073;0.000207073;0.000207073;-0.000338085;0.000207073]
                [-0.00364506;0.002900835;-0.000268379;-0.000268379;-0.000661156;0.002900835;-0.003177794;-0.00364506;0.002900835;-0.000338085;-0.000338085;-0.000338085;-0.000338085;0.002900835;-0.000338085]
                [-0.000434948;-0.000338085;0.000237943;0.000237943;-5.61003E-05;-0.000338085;0.00168961;-0.000434948;-0.000338085;0.000207073;0.000207073;0.000207073;0.000207073;-0.000338085;0.000207073]
            ] |> matrix

        let chol = varcovar.Cholesky().Factor
            *)

        let cholesky =
            [
                [0.134197659;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0]
                [-0.027161877;0.046508791;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0]
                [0.110666451;0.058860481;0.132477986;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0]
                [0.110666451;0.058860481;0.132477986;-1.38778E-17;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0]
                [-0.054754806;-0.046193408;0.014949691;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0]
                [-0.027161877;0.046508791;2.046E-18;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0]
                [0.106204225;-0.006301774;0.325697576;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0]
                [0.134197659;-3.72988E-17;5.23777E-17;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0]
                [-0.027161877;0.046508791;2.046E-18;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0]
                [-0.003241097;-0.009162131;0.008574341;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0]
                [-0.003241097;-0.009162131;0.008574341;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0]
                [-0.003241097;-0.009162131;0.008574341;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0]
                [-0.003241097;-0.009162131;0.008574341;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0]
                [-0.027161877;0.046508791;2.046E-18;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0]
                [-0.003241097;-0.009162131;0.008574341;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0]
            ] |> matrix

        let normal = MathNet.Numerics.Distributions.Normal(0.0, 1.0)
        
        let valuesForSceneraio () =
            let vs =
                [
                    [normal.Sample()];[normal.Sample()];[normal.Sample()]
                    [normal.Sample()];[normal.Sample()];[normal.Sample()]
                    [normal.Sample()];[normal.Sample()];[normal.Sample()]
                    [normal.Sample()];[normal.Sample()];[normal.Sample()]
                    [normal.Sample()];[normal.Sample()];[normal.Sample()]
                ] |> matrix // se generan valores a partir de una distribucion normal estándar (no se muestra el código)
            cholesky * vs // se añade la varianza y covarianza a los valores mediante la descomposición de Cholesky
        let stockPrices = Array.zeroCreate 1000000
        [0 .. 999999] // Se generan un millón de escenarios
        |> Seq.iter(fun i ->
            let vs = valuesForSceneraio()
            let wacc, growth, modelVariablesYear1, modelVariablesYear2, modelVariablesYear3, modelVariablesYear4, modelVariablesYear5 =
                valuesForSimulation
                    (vs.At(0,0)) (vs.At(1,0)) (vs.At(2,0)) (vs.At(3,0)) (vs.At(4,0))
                    (vs.At(5,0)) (vs.At(6,0)) (vs.At(7,0)) (vs.At(8,0)) (vs.At(9,0))
                    (vs.At(10,0)) (vs.At(11,0)) (vs.At(12,0)) (vs.At(13,0)) (vs.At(15,0))
            let stockPrice = 
                valuate wacc growth modelVariablesYear1 modelVariablesYear2 modelVariablesYear3 modelVariablesYear4 modelVariablesYear5
                |> float
            stockPrices.[i] <- if stockPrice < 0. then 0. else stockPrice // si el valor de la acción da negativo, se sustitye por cero
        )
        let ds = MathNet.Numerics.Statistics.DescriptiveStatistics(stockPrices, true)
        printfn "mean - %A\nstdev - %A\nskewness - %A\nkurtosis - %A\nmin - %A\nmax - %A" ds.Mean ds.StandardDeviation ds.Skewness ds.Kurtosis ds.Minimum ds.Maximum
        Chart.Histogram(stockPrices,LowerBound=ds.Minimum,UpperBound=550.0,Intervals=50.)
        |> Chart.Show