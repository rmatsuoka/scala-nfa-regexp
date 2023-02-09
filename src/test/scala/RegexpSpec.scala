
import org.scalatest.funspec.AnyFunSpec
import com.example.regexp._

class RegexpSpec extends AnyFunSpec {

    describe("A regexp 'ab*c'") {
        val re = Regexp.compile("ab*c")
        it("should match following strings") {
            assert(re.matchString("ac"))
            assert(re.matchString("abc"))
            assert(re.matchString("abbbbbbbbc"))
            assert(re.matchString("acXXXXX"))
            assert(re.matchString("XXXXXXXXabbbbbbc"))
            assert(re.matchString("XXXXXabbbbbbbbbbbbbbbbcXXXXXX"))
        }

        it("should not match following strings") {
            assert(re.matchString("") == false)
            assert(re.matchString("ab") == false)
            assert(re.matchString("abbbbbbbbbb") == false)
            assert(re.matchString("bc") == false)
            assert(re.matchString("cba") == false)
            assert(re.matchString("addddc") == false)
            assert(re.matchString("XXXXXXXXXabXXXXXXXX") == false)
        }
    }

    describe("A regexp '(abc|def)'") {
        val re = Regexp.compile("(abc|def)+")
        it("should match following strings") {
            assert(re.matchString("abc"))
            assert(re.matchString("def"))
            assert(re.matchString("abcdef"))
            assert(re.matchString("defabc"))
            assert(re.matchString("_______defdefdef____"))
        }

        it("should not match following strings") {
            assert(re.matchString("") == false)
            assert(re.matchString("ab") == false)
            assert(re.matchString("cde") == false)
            assert(re.matchString("_____") == false)
        }
    }
}