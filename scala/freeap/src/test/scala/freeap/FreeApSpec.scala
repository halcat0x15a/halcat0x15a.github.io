package freeap

import org.scalatest.FunSpec

class FreeApSpec extends FunSpec {

  describe("FreeAp") {

    it("should parse command line arguments") {
      assert(Opt.runParser(User.user, "--username halcat0x15a --fullname SanshiroYoshida --id 346") == Some(User("halcat0x15a", "SanshiroYoshida", 346)))
      assert(Opt.runParser(User.user, "--id 346 --username halcat0x15a --fullname SanshiroYoshida") == Some(User("halcat0x15a", "SanshiroYoshida", 346)))
      assert(Opt.runParser(User.user, "--username halcat0x15a --id 346") == Some(User("halcat0x15a", "", 346)))
      assert(Opt.runParser(User.user, "--fullname SanshiroYoshida --id 346") == None)
    }

  }

}
