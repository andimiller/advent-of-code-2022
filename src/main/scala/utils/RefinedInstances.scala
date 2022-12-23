package utils

import cats.implicits.*
import cats.data.*
import eu.timepit.refined.api.{Result, Validate}
import eu.timepit.refined.collection.Size

object RefinedInstances:
  given [T, N <: Int: ValueOf]: Validate[NonEmptyList[T], Size[N]]      =
    Validate.instance({ nel => Result.fromBoolean(nel.size == valueOf[N], nel) }, _.toString)
  given [K, V, N <: Int: ValueOf]: Validate[NonEmptyMap[K, V], Size[N]] =
    Validate.instance({ nem => Result.fromBoolean(nem.length == valueOf[N], nem) }, _.toString)
