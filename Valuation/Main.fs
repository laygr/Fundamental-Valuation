namespace CompanyValuation
module Main =
    open DiffSharp.AD.Float64
    open MathNet.Numerics.Statistics
    open FSharp.Charting

    (*
    let simulation (values0:DV) =
        let normal = MathNet.Numerics.Distributions.Normal(0., 0.02)

        let addTo (vector:float[]) index quantity =
            vector.[index] <- vector.[index] + quantity

        let values () =
            let newValues = (values0.Copy()).ToArray() |> Array.map float
            for i in [``sales growth index``..``long run growth rate index``] do
                addTo newValues i (normal.Sample())
                //cholesky
            toDV newValues

        let equityValues = [| for _ in 1 .. 1000000 -> enterpriseValue5Years (values()) |> float |]

        let ds = MathNet.Numerics.Statistics.DescriptiveStatistics(equityValues, true)
        printfn "mean - %A\nstdev - %A\nskewness - %A\nkurtosis - %A" ds.Mean ds.StandardDeviation ds.Skewness ds.Kurtosis

        Chart.Histogram(equityValues,LowerBound=equityValues.Minimum(),UpperBound=equityValues.Maximum(),Intervals=40.)
        |> Chart.Show
    *)

    [<EntryPoint>]
    let main argv =
        // csvGradients values0
        // simulation values0
        (*
        let stockValue, gradient, elasticities = TelevisaValuation.valuate

        [0..elasticities.Length-1]
        |> Seq.iter (fun i ->
            printf "%f\n" (float elasticities.[i])
        )
        *)
        let covar = Cholesky.simulation()
        0