import chisel3.stage._
import sim._

object TopMain extends App {
  (new ChiselStage).execute(
    args,
    Seq(
      ChiselGeneratorAnnotation(() => new SimTop())
    )
  )
  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => new ysyx_210285()),
    firrtl.stage.RunFirrtlTransformAnnotation(new AddModulePrefix()),
    ModulePrefixAnnotation("ysyx_210285_")
  ))
}
