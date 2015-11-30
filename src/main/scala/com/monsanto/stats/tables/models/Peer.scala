package com.monsanto.stats.tables.models

object PeerType {

  val Follower = 1
  val Friend = 2

}

case class Peer(
  originalUserId: Long,
  peerType: Int,
  peerUserId: Long
)
