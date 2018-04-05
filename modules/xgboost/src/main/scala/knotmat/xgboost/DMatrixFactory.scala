package knotmat.xgboost

import knotmat.{DataSet, ModelFactory}
import ml.dmlc.xgboost4j.java.DMatrix
import scala.collection.mutable

object DMatrixFactory {

  object Dense {

    def singleLabel: ModelFactory.Dense.SingleLabel[Float, DMatrix] =
      ModelFactory[DMatrix].compose[DataSet.Dense.SingleLabel[Float]]({ dataSet =>
        val features = mutable.Buffer.empty[Array[Float]]
        val labels   = Array.newBuilder[Float]
        val (nrows, ncols) =
          dataSet.foreach({ point =>
            features += point.feature.chunks
            labels   += point.label.value
          })
        val data = new Array[Float](features.foldLeft(0)(_ + _.length))
        var i = 0
        for (f <- features) {
          System.arraycopy(f, 0, data, i, f.length)
          i += f.length
        }
        val dm = new DMatrix(data, nrows, ncols)
        dm.setLabel(labels.result())
        dm
      })

  }

}
