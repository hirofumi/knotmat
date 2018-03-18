package knotmat.xgboost

import knotmat.ModelFactory
import ml.dmlc.xgboost4j.java.DMatrix

object DMatrixFactory {

  def denseLabeled: ModelFactory.ForLabeledSet[Array[Float], Float, DMatrix] =
    ModelFactory[DMatrix]
      .composeLabeledSet[Array[Float], Float]({ labeled =>
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
