
import org.scalatest.funspec.AnyFunSpec
import com.example.regexp._

class RegexpSpec extends AnyFunSpec {
    val re = Regexp.compile("ab*c")
    describe("A regexp") {
        it("should match these strings") {
            assert(re.matchString("ac"))
            assert(re.matchString("abc"))
            assert(re.matchString("abbbbbbbbc"))
        }
    }
}