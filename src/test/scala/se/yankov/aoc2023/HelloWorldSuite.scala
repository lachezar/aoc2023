package se.yankov.aoc2023

import cats.effect.{ IO, SyncIO }
import munit.CatsEffectSuite

class HelloWorldSuite extends CatsEffectSuite {

  test("test hello world says hi") {
    assertEquals(1, 1)
  }
}
