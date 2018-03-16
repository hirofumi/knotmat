package knotmat.xgboost

import knotmat.LabeledSetCodec
import ml.dmlc.xgboost4j.java.DMatrix

object DMatrixCodec {

  def denseLabeled: LabeledSetCodec.Simple[DMatrix, Array[Float], Float] =
    LabeledSetCodec[Array[Float], Float]
      .map({ labeled =>
        val features = Array.newBuilder[Float]
        val labels   = Array.newBuilder[Float]
        val (nrows, ncols) =
          labeled.foreach({
            case (feature, label) =>
              features ++= feature
              labels    += label
          })
        val dm = new DMatrix(features.result(), nrows, ncols)
        dm.setLabel(labels.result())
        dm
      })

}
