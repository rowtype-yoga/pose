package purescript_aff

import (
	"errors"
	"fmt"
	"os"
	"reflect"
	"runtime"
	"strings"
	"sync"
	"time"

	. "github.com/purescript-native/go-runtime"
)

// Whether to log errors or not
const TRACE = false

func Trace(stuff ...Any) {
	if TRACE {
		fmt.Println(stuff...)
	}
}

func dontRecover() Any {
	return nil
}
func TraceAnyway(stuff ...Any) {
	fmt.Println(stuff...)
}

var (
	Info = Teal
	Warn = Yellow
	Fata = Red
)

var (
	Black   = Color("\033[1;30m%s\033[0m")
	Red     = Color("\033[1;31m%s\033[0m")
	Green   = Color("\033[1;32m%s\033[0m")
	Yellow  = Color("\033[1;33m%s\033[0m")
	Purple  = Color("\033[1;34m%s\033[0m")
	Magenta = Color("\033[1;35m%s\033[0m")
	Teal    = Color("\033[1;36m%s\033[0m")
	White   = Color("\033[1;37m%s\033[0m")
)

func Color(colorString string) func(...interface{}) string {
	sprint := func(args ...interface{}) string {
		return fmt.Sprintf(colorString,
			fmt.Sprint(args...))
	}
	return sprint
}

type Fiber = Dict

// Pure a
type Pure struct {
	Pure  bool
	value Any
}

// type memo interface {
// 	getMemo() Any
// 	setMemo(v Any)
// }

// func (p Pure) getMemo() Any {
// 	return p.value
// }

// func (p Pure) setMemo(m Any) {
// 	p.value = m
// }

// func (p AbstractAff) getMemo() Any {
// 	return p._3
// }

// func (p AbstractAff) setMemo(m Any) {
// 	p._3 = m
// }

func (p Pure) String() string {
	return fmt.Sprintf("Pure %s", p.value)
}

// Throw Error
type Throw struct {
	Throw bool
	err   error
}

func (t Throw) String() string {
	return fmt.Sprintf("Throw %s", t.err)
}

// Catch (Aff a) (Error -> Aff a)
type Catch struct {
	Catch      bool
	aff        Any
	errorToAff func(Any) Any
}

func (c Catch) String() string {
	return fmt.Sprintf("Catch %s", c.aff)
}

// Sync (Effect a)
type Sync struct {
	Sync bool
	eff  func() Any
}

func (s Sync) String() string {
	return fmt.Sprintf("Sync")
}

type K = Any
type Unit = Any
type EffectUnit = func() Unit
type EitherErrorOrAToEffectUnit = func(Any) EffectUnit
type EffectCanceler = func() Canceler
type Canceler = func(err Any) Any
type AsyncFn = func(EitherErrorOrAToEffectUnit) EffectCanceler

// Async ((Either Error a -> Effect Unit) -> Effect (Canceler))
type Async struct {
	Async   bool
	asyncFn Any
}

func (a Async) String() string {
	return fmt.Sprintf("Async")
}

// forall b. Bind (Aff b) (b -> Aff a)
type Bind struct {
	Bind   bool
	affOfB Any
	bToAff func(Any) Any
}

func (b Bind) String() string {
	return fmt.Sprintf("Bind %d", b.affOfB)
}

// forall b. Bracket (Aff b) (BracketConditions b) (b -> Aff a)
type Bracket struct {
	Bracket           bool
	acquire           Any
	bracketConditions Dict
	k                 func(Any) Any
}

func (b Bracket) String() string {
	return fmt.Sprintf("Bracket acquire:%d conditions:%d",
		b.acquire,
		b.bracketConditions)
}

// forall b. Fork Boolean (Aff b) ?(Fiber b -> a)
type Fork struct {
	Fork      bool
	immediate bool
	affOfB    Any
}

func (f Fork) String() string {
	return fmt.Sprintf("Fork %d", f.affOfB)
}

// Sequential (ParAff a)
type Sequential struct {
	Sequential bool
	parAff     Any
}

type Return struct {
	Return bool
}

func (r Return) String() string {
	return fmt.Sprintf("Return")
}

type Resume struct {
	Resume bool
	b      Cons
}

func (r Resume) String() string {
	return fmt.Sprintf("Resume %d", r.b.head)
}

type Release struct {
	Release           bool
	bracketConditions Dict
	result            Any
}

func (r Release) String() string {
	return fmt.Sprintf("Release %d", r.result)
}

type Finalizer struct {
	Finalizer bool
	finalizer Any
}

func (f Finalizer) String() string {
	return fmt.Sprintf("Finalizer")
}

type Finalized struct {
	Finalizer bool
	step      Any
	fail      Any
}

func (f Finalized) String() string {
	return fmt.Sprintf("Finalized %d %d", f.step, f.fail)
}

type AbstractAff struct {
	tag string
	_1  Any
	_2  Any
	_3  Any
}

func (aa AbstractAff) String() string {
	return fmt.Sprintf("\nAff:\t%s\n\t_1:  %d\n\t_2:  %d\n\t_3:  %d",
		aa.tag, aa._1, aa._2, aa._3)
}

var Empty AbstractAff = AbstractAff{tag: "empty"}

// forall b. Map (b -> a) (ParAff b)
// forall b. Apply (ParAff (b -> a)) (ParAff b)
// Alt (ParAff a) (ParAff a)
type ParAlt struct {
	parAffFn Any
	parAff   Any
	memo     Any
}

type OnComplete = Dict

type Cons struct {
	head Any
	tail *Cons
}

func printCons(cons *Cons) {
	var goFn func(depth int, currHead Any, currTail *Cons) = nil
	goFn = func(depth int, currHead Any, currTail *Cons) {
		if currHead != nil {
			Trace(Yellow(currHead))
			if currTail != nil {
				goFn(depth+1, currTail.head, currTail.tail)
			}
		}
	}
	goFn(0, cons.head, cons.tail)
}

type InterruptCons struct {
	head      Any
	interrupt *Fiber
	tail      *InterruptCons
}

const SUSPENDED = 0   // Suspended, pending a join.
const CONTINUE = 1    // Interpret the next instruction.
const STEP_BIND = 2   // Apply the next bind.
const STEP_RESULT = 3 // Handle potential failure from a result.
const PENDING = 4     // An async effect is running.
const RETURN = 5      // The current stack has returned.
const COMPLETED = 6   // The entire fiber has completed.

func statusName(st int) string {
	switch st {
	case SUSPENDED:
		return "SUSPENDED"
	case CONTINUE:
		return "CONTINUE"
	case STEP_BIND:
		return "STEP_BIND"
	case STEP_RESULT:
		return "STEP_RESULT"
	case PENDING:
		return "PENDING"
	case RETURN:
		return "RETURN"
	case COMPLETED:
		return "COMPLETED"
	}
	return "!!!!! UNKNOWN !!!!! "
}

func nonCanceler(err Any) Any {
	return Pure{Pure: true, value: nil}
}

func runEff(eff func() Any) {
	Trace(Teal("func: runEff"))
	defer func() {
		if err := recover(); err != nil {
			Trace(Magenta("Caught error in runEff: ", err))
			var msg string
			switch errTyped := err.(type) {
			case error:
				msg = errTyped.Error()
			case string:
				msg = errTyped
			default:
				Trace(Red("Unexpected error type in runEff ", reflect.TypeOf(err), err))
				os.Exit(0)
			}
			if msg == "runtime error: invalid memory address or nil pointer dereference" {
				Trace(Red("invalid mem", msg))
				panic(err)
			}
			if strings.HasPrefix(msg, "interface conversion:") {
				Trace(Red("Wrong cast ", msg))
				panic(err)
			}
			if strings.HasPrefix(msg, "Failed pattern match") {
				Trace(Red("FPM ", msg))
				panic(err)
			}
			Trace("\tError> ", err)
			go func() {
				panic(err)
			}()
		}
	}()
	eff()
}

func runSync(left func(Any) Any, right func(Any) Any, eff func() Any) Any {
	Trace(Teal("func: runSync with ", reflect.TypeOf(eff)))
	var res Any = left(errors.New("Error wasn't set properly"))
	defer func() {
		if err := recover(); err != nil {
			Trace(Magenta("Caught error in runSync: ", err))
			var msg string
			switch errTyped := err.(type) {
			case error:
				msg = errTyped.Error()
			case string:
				msg = errTyped
			default:
				Trace(Red("Unexpected error type in runSync ", reflect.TypeOf(err), err))
				os.Exit(0)
			}
			if msg == "runtime error: invalid memory address or nil pointer dereference" {
				Trace(Red("invalid mem", msg))
				panic(err)
			}
			if strings.HasPrefix(msg, "interface conversion:") {
				Trace(Red("Wrong cast ", msg))
				panic(err)
			}
			Trace("\tError> ", err)
			res = left(errors.New(msg))
		}
	}()
	res = right(eff())
	return res
}

func runAsync(left func(Any) Any, eff Any, k K) Any {
	Trace(Teal("func: runAsync"))
	// catch...
	var canceler Any = nonCanceler
	defer func() {
		if err := recover(); err != nil {
			Trace(Magenta("Caught error in runAsync: ", err))
			var msg string
			switch errTyped := err.(type) {
			case error:
				msg = errTyped.Error()
			case string:
				msg = errTyped
			case func(Any) Any:
				Trace(Red("Fn error type in runAsync ", errTyped(nil)))
				panic(err)
			default:
				Trace(Red("Unexpected error type in runAsync ", reflect.TypeOf(err), err))
				os.Exit(0)
			}
			if msg == "runtime error: invalid memory address or nil pointer dereference" {
				Trace(Red("invalid mem", msg))
				panic(err)
			}
			if strings.HasPrefix(msg, "interface conversion:") {
				Trace(Red("Wrong cast ", msg))
				panic(err)
			}
			Trace("\tError> ", err)
			k.(func(Any) Any)(left(err)).(func() Any)()
		}
	}()
	canceler = eff.(func(Any) Any)(k).(func() Any)()
	return canceler
}

func sequential(util Any, supervisor *Supervisor, par Any) Async {
	return Async{asyncFn: func(cb Any) Any {
		return func() Any {
			return runPar(util, supervisor, par, cb)
		}
	},
	}
}

type Scheduler struct {
	isDraining func() bool
	enqueue    func(cb func())
}

var scheduler Scheduler = (func() Scheduler {
	const limit = 1024
	size := 0
	ix := 0
	var queue [limit](func())
	draining := false

	drain := func() {
		Trace(Teal("func: drain", size))
		draining = true
		for size != 0 {
			size--
			thunk := queue[ix]
			ix = (ix + 1) % limit
			thunk()
		}
		draining = false
	}

	isDraining := func() bool { return draining }
	enqueue := func(cb func()) {
		Trace(Teal("func: enqueue"))
		if size == limit {
			tmp := draining
			drain()
			draining = tmp
		}
		queue[(ix+size)%limit] = cb
		size++

		if !draining {
			drain()
		}
	}

	return Scheduler{isDraining: isDraining, enqueue: enqueue}
})()

type Supervisor = struct {
	register func(fiber Fiber)
	isEmpty  func() bool
	killAll  func(killError error, cb func() Any) Any
}

func mkSupervisor(util_ Any) Supervisor {
	// Boilerplate
	var util map[string]Any = util_.(map[string]Any)
	var isLeft func(Any) Any = util["isLeft"].(func(Any) Any)
	var fromLeft func(Any) Any = util["fromLeft"].(func(Any) Any)

	// Start here
	fibers := NewRegularIntMap()
	var fiberID = 0
	var count = 0
	return Supervisor{
		register: func(fiber Fiber) {
			Trace("func: supervisor.register", fiber)
			fid := fiberID
			fiberID++
			fiber["onComplete"].(func(Dict) Any)(
				Dict{
					"rethrow": true,
					"handler": func(result Any) Any {
						return func() Any {
							Trace(Green("🔛✅ Registered on complete handler running"))
							count--
							fibers.Delete(fid)
							return nil
						}
					},
				}).(func() Any)()
			fibers.Store(fid, fiber)
			count++
		},
		isEmpty: func() bool {
			Trace("func: supervisor.isEmpty")
			return count == 0
		},
		killAll: func(killError error, cb func() Any) Any {
			return func() Any {
				Trace("func: supervisor.killAll; count = ", count)
				if count == 0 {
					Trace("\n\nreturning cb")
					return cb()
				}
				killCount := 0
				kills := map[int](func() Any){}

				kill := func(fid int, fib Fiber) {
					Trace(Red("\tMaking killFn for fiber with id ", fiberID))
					kills[fid] = fib["kill"].(func(Any, Any) Any)(killError, func(result Any) Any {
						return func() Any {
							delete(kills, fid)
							killCount--
							if isLeft(result).(bool) && fromLeft(result) != nil {
								go func() {
									panic(fromLeft(result)) // [TODO] Panic better?
								}()

							}
							if killCount == 0 {
								Trace(Red("\tCalling cb"))
								cb()
							}
							return nil
						}
					}).(func() Any)().(func() Any)
				}

				// [TODO] unsafe iteration
				for fiberID, _ := range fibers.internal {
					killCount++
					fib, nope := fibers.Load(fiberID)
					Trace(nope, "Just Chuck Testa")
					kill(fiberID, fib.(Dict))
				}

				fibers = NewRegularIntMap()
				fiberID = 0
				count = 0

				return func(err Any) Any {
					return Sync{
						eff: func() Any {
							for _, killThunk := range kills {
								killThunk()
							}
							return nil
						}}
				}

			}
		},
	}
}

func mkFiber(util_ Any, supervisor *Supervisor, aff_ Any) Fiber {
	var util map[string]Any = util_.(map[string]Any)
	var isLeft func(Any) Any = util["isLeft"].(func(Any) Any)
	var left func(Any) Any = util["left"].(func(Any) Any)
	var right func(Any) Any = util["right"].(func(Any) Any)
	var fromRight func(Any) Any = util["fromRight"].(func(Any) Any)
	var fromLeft func(Any) Any = util["fromLeft"].(func(Any) Any)
	runTick := 0

	stepLock := &sync.Mutex{}
	step := aff_ // Successful step
	setStep := func(who string, newStep Any) {
		stepLock.Lock()
		Trace(who, "\n\tfrom:\t", step, "\n\tto:\t", newStep)
		step = newStep
		stepLock.Unlock()
	}
	var fail Any              // Failure step
	var interrupt *Dict = nil // Asynchronous interrupt

	// Stack of continuations for the current fiber.
	var b Cons = Cons{}

	// Stack of attempts and finalizers for error recovery. Every `Cons` is also
	// tagged with current `interrupt` state. We use this to track which items
	// should be ignored or evaluated as a result of a kill.
	var attempts *InterruptCons = &InterruptCons{}

	// A special state is needed for Bracket, because it cannot be killed. When
	// we enter a bracket acquisition or finalizer, we increment the counter,
	// and then decrement once complete.
	bracketCount := 0

	// Each join gets a new id so they can be revoked.
	joinId := 0
	var joins map[int]OnComplete = nil
	var rethrow = true

	status := SUSPENDED

	var run func(int) Any
	run = func(localRunTick int) Any {

		Trace("New round", "localRunTick", localRunTick, "bracketCount", bracketCount)
		var tmp *Dict
		var result Any
		var attempt Any
		for {
			Trace(Purple("\n=======================\nStatus:\t", statusName(status), "\nStep:\t", step, "\n======================="))
			tmp = nil
			result = nil
			attempt = nil
			switch status {
			case STEP_BIND:
				Trace("STEP_BIND", b.head)
				status = CONTINUE // next step
				headFn := b.head.(func(Any) Any)
				// Makeshift try catch block
				func() {
					defer func() {
						if rawErr := recover(); rawErr != nil {
							err, ok := rawErr.(error)
							if !ok {
								Trace(Red("\tNon error error ", rawErr))
								panic(error(errors.New("Error of unexpected type")))
							}
							if err.Error() == "runtime error: invalid memory address or nil pointer dereference" {
								Trace(Red("Invalid cast in STEP_BIND", err))
								panic(err)
							}
							if strings.HasPrefix(err.Error(), "interface conversion:") {
								Trace(Red("Wrong cast in STEP_BIND ", err))
								panic(err)
							}
							Trace(Red("\tError oh shit ", err))
							// early return on error
							status = RETURN
							fail = left(err)
							// step = nil
							setStep("bind error", nil)
						}
					}()
					// step = headFn(step)
					setStep("bind success", headFn(step))
					Trace("\t> Next step", step)
					if b.tail == nil {
						b.head = nil
					} else {
						b.head = b.tail.head
						b.tail = b.tail.tail
					}
				}()
			case STEP_RESULT:
				Trace("STEP_RESULT", step, reflect.TypeOf(step))
				if isLeft(step).(bool) {
					// early return on error
					status = RETURN
					fail = step
					// step = nil
					setStep("result", nil)
				} else if b.head == nil {
					// happy case done
					status = RETURN
					Trace("My work here is done")
				} else {
					// happy case work left
					status = STEP_BIND
					// step = fromRight(step)
					setStep("result", fromRight(step))
					Trace("Next step after happy result", step)
				}

			case CONTINUE:
				Trace("CONTINUE")
				switch currentStep := step.(type) {

				case Bind:
					Trace("\tBind")
					if b.head != nil {
						b.tail = &Cons{head: b.head, tail: b.tail}
					}
					b.head = currentStep.bToAff
					status = CONTINUE
					// step = currentStep.affOfB
					setStep("continue", currentStep.affOfB)

				case Pure:
					Trace("\tPure")
					if b.head == nil {

						Trace("\t> Head nil")
						// we're done
						status = RETURN
						// step = right(currentStep.value)
						setStep("pure, return", right(currentStep.value))
					} else {
						Trace("\t> Head exists", currentStep.value)
						// this happens after a bind
						// step = currentStep.value
						setStep("pure, bind", currentStep.value)
						status = STEP_BIND
					}

				case Sync:
					Trace("\tSync")
					// step = runSync(left, right, currentStep.eff)
					setStep("sync", runSync(left, right, currentStep.eff))
					status = STEP_RESULT

				case Async:
					Trace("\tAsync")
					status = PENDING
					var asyncCb K = func(theResult Any) Any {
						return func() Unit {
							if runTick != localRunTick {
								Trace("\t\tRun tick != localRunTick")
								return nil
							}
							runTick++
							scheduler.enqueue(func() {
								if runTick != localRunTick+1 {
									Trace("\t\tRun tick != localRunTick + 1")
									return
								}
								// step = theResult
								setStep("Async Callback", theResult)
								status = STEP_RESULT
								run(runTick)
							})
							return nil
						}
					}
					// runAsync returns a canceller
					// step = runAsync(left, currentStep.asyncFn, asyncCb)
					setStep("Async ", runAsync(left, currentStep.asyncFn, asyncCb))
					return nil

				case Throw:
					Trace("\tThrow")
					status = RETURN
					fail = left(currentStep.err)
					// step = nil
					setStep("Throw", nil)

				case Catch:
					Trace("\tCatch", currentStep.aff)
					if b.head == nil {
						Trace("\t\tHead is nil")
						attempts = &InterruptCons{head: step, tail: attempts, interrupt: interrupt}
					} else {
						Trace("\t\tHead is not nil", b.head)
						attempts = &InterruptCons{
							interrupt: interrupt,
							head:      step,
							tail: &InterruptCons{
								interrupt: interrupt,
								head:      Resume{b: b},
								tail:      attempts}}
					}
					b.head = nil
					b.tail = nil
					status = CONTINUE
					// step = currentStep.aff
					setStep("Catch", currentStep.aff)

				// Enqueue the Bracket so that we can call the appropriate handlers
				// after resource acquisition.
				case Bracket:
					Trace("\tBracket")
					bracketCount++
					if b.head == nil {
						Trace("\t\tb.head == nil")
						attempts = &InterruptCons{head: step, tail: attempts, interrupt: interrupt}
					} else {
						Trace("\t\tb.head /= nil")
						attempts = &InterruptCons{
							interrupt: interrupt,
							head:      step,
							tail: &InterruptCons{
								interrupt: interrupt,
								head:      Resume{b: b},
								tail:      attempts}}
					}
					b.head = nil
					b.tail = nil
					status = CONTINUE
					// step = currentStep.acquire
					setStep("Bracket", currentStep.acquire)

				case Fork:
					Trace("\tFork")
					tmpVar := mkFiber(util, supervisor, currentStep.affOfB)
					tmp = &tmpVar
					if supervisor != nil {
						supervisor.register(*tmp)
					}
					if currentStep.immediate {
						(*tmp)["run"].(func() Any)()
					}
					// step = right(*tmp)
					setStep("Fork", right(*tmp))
					status = STEP_RESULT

				case Sequential:
					Trace("\tSequential")
					status = CONTINUE
					// step = sequential(util, supervisor, currentStep.parAff)
					setStep("Sequential", sequential(util, supervisor, currentStep.parAff))

				default:
					Trace(Purple("Oh god ", reflect.TypeOf(step)))
					panic(step)

				}

			case RETURN:
				Trace("RETURN")
				b.head = nil
				b.tail = nil
				Trace("\tAttempts:", attempts)
				if attempts.head == nil {
					Trace("\tAttempts are nil", interrupt, fail)
					status = COMPLETED
					if interrupt != nil {
						// step = *interrupt
						setStep("return interrupt", *interrupt)
					} else if fail != nil {
						// step = fail
						setStep("return fail", fail)
					}
				} else {
					Trace("\tAttempts aren't nil", attempts.head, attempts.tail)
					tmp = attempts.interrupt
					attempt = attempts.head
					attempts = attempts.tail

					switch currentAttempt := attempt.(type) {
					case Catch:
						Trace("\tReturn Catch")
						if interrupt != nil && interrupt != tmp && bracketCount == 0 {
							Trace("\t\tGonna RETURN")
							status = RETURN
						} else if fail != nil {
							Trace("\t\tGonna CONTINUE", fromLeft(fail))
							status = CONTINUE
							// step = currentAttempt.errorToAff(fromLeft(fail))
							setStep("return catch", currentAttempt.errorToAff(fromLeft(fail)))
							fail = nil
						}

					// We cannot resume from an unmasked interrupt or exception.
					case Resume:
						Trace("\tResume")
						// As with Catch, we only want to ignore in the case of an
						// interrupt since enqueing the item.
						if (interrupt != nil && interrupt != tmp && bracketCount == 0) || fail != nil {
							status = RETURN
						} else {
							b.head = currentAttempt.b.head
							b.tail = currentAttempt.b.tail
							status = STEP_BIND
							// step = fromRight(step)
							setStep("resume ", fromRight(step))
						}

					case Bracket:
						Trace("\tBracket")
						bracketCount--
						if fail == nil {
							result = fromRight(step)
							attempts = &InterruptCons{
								head: Release{
									bracketConditions: currentAttempt.bracketConditions,
									result:            result,
								},
								tail:      attempts,
								interrupt: tmp,
							}

							if interrupt == tmp || bracketCount > 0 {
								Trace("\t\tContinuing")
								status = CONTINUE
								// step = currentAttempt.k(result)
								setStep("Bracket continue", currentAttempt.k(result))
							}
						}
					case Release:
						Trace("\tRelease")
						attempts = &InterruptCons{
							head:      Finalized{step: step, fail: fail},
							tail:      attempts,
							interrupt: interrupt,
						}
						status = CONTINUE
						// It has only been killed if the interrupt status has changed
						// since we enqueued the item, and the bracket count is 0. If the
						// bracket count is non-zero then we are in a masked state so it's
						// impossible to be killed.
						if interrupt != nil && &interrupt != &tmp && bracketCount == 0 {
							Trace("\t\tkilled branch")
							// step = currentAttempt.bracketConditions["killed"].(func(Any) Any)(fromLeft(*interrupt)).(func(Any) Any)(currentAttempt.result)
							setStep("Release killed", currentAttempt.bracketConditions["killed"].(func(Any) Any)(fromLeft(*interrupt)).(func(Any) Any)(currentAttempt.result))
						} else if fail != nil {
							Trace("\t\tfailed branch")
							// step = currentAttempt.bracketConditions["failed"].(func(Any) Any)(fromLeft(fail)).(func(Any) Any)(currentAttempt.result)
							setStep("Release failed", currentAttempt.bracketConditions["failed"].(func(Any) Any)(fromLeft(fail)).(func(Any) Any)(currentAttempt.result))
						} else {
							Trace("\t\tcompleted branch")
							// step = currentAttempt.bracketConditions["completed"].(func(Any) Any)(fromRight(step)).(func(Any) Any)(currentAttempt.result)
							setStep("Release completed", currentAttempt.bracketConditions["completed"].(func(Any) Any)(fromRight(step)).(func(Any) Any)(currentAttempt.result))
						}
						fail = nil
						bracketCount++
					case Finalizer:
						Trace("\tFinalizer")
						bracketCount++
						attempts = &InterruptCons{
							head:      Finalized{step: step, fail: fail},
							tail:      attempts,
							interrupt: interrupt,
						}
						status = CONTINUE
						// step = currentAttempt.finalizer
						setStep("Finalizer", currentAttempt.finalizer)
					case Finalized:
						Trace("\tFinalized")
						bracketCount--
						status = RETURN
						// step = currentAttempt.step
						setStep("Finalized", currentAttempt.step)
						fail = currentAttempt.fail
					default:
						Trace(Fata("\tUnknown Attempt type hit", reflect.TypeOf(currentAttempt)))
					}
				}
			case COMPLETED:
				Trace("COMPLETED", joins)
				for _, join := range joins {
					if rethrow {
						rethrow = join["rethrow"].(bool)
					}
					Trace(Green("\tRunning onComplete ✅ handler"))
					runEff(join["handler"].(func(Any) Any)(step).(func() Any))
				}
				joins = nil
				// If we have an interrupt and a fail, then the thread threw while
				// running finalizers. This should always rethrow in a fresh stack.
				if interrupt != nil && fail != nil {
					go func() {
						// [TODO]: How can we avoid this?
						// [TODO]: Do we need this here?
						// time.Sleep(50 * time.Millisecond) // [TODO]: How can we avoid this?
						go panic(fromLeft(fail))
					}()

					// If we have an unhandled exception, and no other fiber has joined
					// then we need to throw the exception in a fresh stack.
				} else if isLeft(step).(bool) && rethrow {
					go func() {
						// Guard on rethrow because a completely synchronous fiber can
						// still have an observer which was added after-the-fact.
						time.Sleep(80 * time.Millisecond) // [TODO]: How can we avoid this?
						runtime.Gosched()
						if rethrow {
							panic(fromLeft(fail))
						}
					}()
				}
				return nil

			case SUSPENDED:
				Trace("SUSPENDED")
				status = CONTINUE
			case PENDING:
				Trace("PENDING")
				return nil

			default:
				Trace(Fata("Unknown branch hit", step))
			}
		}

	}

	onComplete := func(join OnComplete) Any {
		return func() Any {
			Trace(Teal("func: onComplete"))
			if status == COMPLETED {
				Trace("\tstatus == COMPLETED")
				rethrow = rethrow && join["rethrow"].(bool)
				join["handler"].(func(Any) Any)(step).(func() Any)()
				return func() Any { return nil }
			}
			Trace("\tstatus == ", statusName(status))

			var jid = joinId
			Trace("\tJoin id", jid)
			joinId++
			if joins == nil {
				joins = map[int]OnComplete{}
			}
			joins[jid] = join

			return func() Any {
				if joins != nil {
					delete(joins, jid)
				}
				return nil
			}
		}
	}

	kill := func(err Any, cb_ Any) Any {
		cb := cb_.(func(Any) Any)
		return func() Any {
			if status == COMPLETED {
				Trace(Green("Completed, no need to cancel"))
				cb(right(nil)).(func() Any)()
				return func() Any { return nil }
			}

			var canceler (func() Any) = onComplete(Dict{
				"rethrow": false,
				"handler": func(_ Any) Any {
					return cb(right(nil))
				},
			}).(func() Any)().(func() Any)

			switch status {
			case SUSPENDED:
				interruptValue := left(err).(Dict)
				interrupt = &interruptValue
				status = COMPLETED
				step = *interrupt
				run(runTick)
			case PENDING:
				if interrupt == nil {
					interruptValue := left(err).(Dict)
					interrupt = &interruptValue
				}
				if bracketCount == 0 {
					if status == PENDING {
						attempts = &InterruptCons{
							head:      Finalizer{finalizer: step.(func(Any) Any)(err)},
							tail:      attempts,
							interrupt: interrupt}
					}
					status = RETURN
					step = nil
					fail = nil
					runTick++
					run(runTick)
				}
			default:
				if interrupt == nil {
					interruptValue := left(err).(Dict)
					interrupt = &interruptValue
				}
				if bracketCount == 0 {
					status = RETURN
					step = nil
					fail = nil
				}
			}

			return canceler
		}
	}

	join := func(cb K) Any {
		return func() Any {
			Trace(Teal("func: join 1"))
			Trace("\tjoin: status: ", statusName(status))
			canceler := onComplete(
				Dict{
					"rethrow": false,
					"handler": cb.(func(Any) Any),
				}).(func() Any)()
			if status == SUSPENDED {
				Trace("join: \tcalling run")
				run(runTick)
			}
			return canceler
		}
	}

	return Fiber{
		"run": func() Any {
			if status == SUSPENDED {
				if !scheduler.isDraining() {
					scheduler.enqueue(func() {
						run(runTick)
					})
				} else {
					run(runTick)
				}
			}
			return nil // Important to avoid panic
		},
		"kill":        kill,
		"join":        join,
		"onComplete":  onComplete,
		"isSuspended": func() Any { return status == SUSPENDED },
	}
}

func runPar(util_ Any, supervisor *Supervisor, par Any, cb Any) Any {

	var util map[string]Any = util_.(map[string]Any)
	var isLeft func(Any) Any = util["isLeft"].(func(Any) Any)
	var left func(Any) Any = util["left"].(func(Any) Any)
	var right func(Any) Any = util["right"].(func(Any) Any)
	var fromRight func(Any) Any = util["fromRight"].(func(Any) Any)
	var fromLeft func(Any) Any = util["fromLeft"].(func(Any) Any)

	fiberID := 0
	fibers := NewRegularIntMap()

	killID := 0
	kills := NewRegularIntMap()

	early := errors.New("[ParAff] Early exit")

	var interrupt Any = nil

	var root Any = Empty

	kill := func(err error, par *AbstractAff, cb K) Any {
		step := par
		var head *AbstractAff
		var tail *Cons

		count := 0
		kills := NewRegularIntMap()
		var tmp Any
		var kid int

	loop:
		for {
			tmp = nil
			Trace("Step", reflect.TypeOf(step))

			switch step.tag {
			case "Forked":
				if step._3 == Empty {
					tmp, _ = fibers.Load(step._1.(int))
					tmpKillFn := tmp.(Fiber)["kill"].(func(Any, Any) Any)
					kills.Store(count, tmpKillFn(err, func(result Any) Any {
						return func() Any {
							count--
							if count == 0 {
								cb.(func(Any) Any)(result).(func() Any)()
							}
							return nil
						}
					}))
					count++
				}
				if head == nil {
					break loop
				}

				step = head._2.(*AbstractAff)
				if tail == nil {
					head = nil
				} else {
					head = tail.head.(*AbstractAff)
					tail = tail.tail
				}
			case "ParMap":
				step = step._2.(*AbstractAff)
			case "ParApply":
				if head != nil {
					tail = &Cons{head: head, tail: tail}
				}
				head = step
				step = step._1.(*AbstractAff)
			case "ParAlt":
				if head != nil {
					tail = &Cons{head: head, tail: tail}
				}
				head = step
				step = step._1.(*AbstractAff)
			}
		}
		if count == 0 {
			cb.(func(Any) Any)(right(nil)).(func() Any)()
		} else {
			// Run the cancelation effects. We alias `count` because it's mutable.
			tmp = count
			for kid = 0; kid < tmp.(int); kid++ {
				kill, ok := kills.Load(kid)
				if ok {
					kills.Store(kid, kill.(func() Any)())
				}

			}
		}
		return kills
	}

	var join func(result Any, cons *Cons)
	join = func(result Any, cons *Cons) {
		Trace("Join")
		var head *AbstractAff
		var tail *Cons
		if cons == nil {
			head = nil
			cons = nil
		} else {
			head = cons.head.(*AbstractAff)
			tail = cons.tail
		}
		Trace(Teal("func: join 2 ", result, head))
		var fail Any
		var step Any
		var lhs Any
		var rhs Any
		var tmp Any
		var kid Any

		if isLeft(result).(bool) {
			fail = result
			step = nil
		} else {
			step = result
			fail = nil
		}

		for {
			Trace(Magenta("\n############\njoinPar:\t", head, "\nstep:\t", step, "\n#############"))
			// printCons(cons)
			Trace(head)
			Trace("---------\n")
			lhs = nil
			rhs = nil
			tmp = nil
			kid = nil

			// We should never continue if the entire tree has been interrupted.
			if interrupt != nil {
				return
			}

			// We've made it all the way to the root of the tree, which means
			// the tree has fully evaluated.
			if head == nil {
				var cbArg Any = nil
				if fail != nil {
					cbArg = fail
				} else {
					cbArg = step
				}
				cb.(func(Any) Any)(cbArg).(func() Any)()
				return
			}

			if head._3 != Empty {
				return
			}

			switch head.tag {

			case "ParMap":
				Trace("Eating ParMap")
				if fail == nil {
					// Unwrap the Right and apply the function in the map and
					// stick it back in a Right
					head._3 = right(head._1.(func(Any) Any)(fromRight(step)))
					step = head._3
				} else {
					head._3 = fail
				}
			case "ParApply":
				Trace("Eating ParApply")
				lhs = head._1.(*AbstractAff)._3
				rhs = head._2.(*AbstractAff)._3
				// If we have a failure we should kill the other side because we
				// can't possible yield a result anymore.
				if fail != nil {
					head._3 = fail
					tmp = true
					kid = killID
					killID++

					var someTmp Any
					if isLeft(lhs).(bool) && (fromLeft(fail) == fromLeft(lhs)) {
						someTmp = head._2
					} else {
						someTmp = head._1
					}
					kills.Store(kid.(int), kill(early.(error), someTmp.(*AbstractAff), func(unused Any) Any {
						return func() Any {
							kills.Delete(kid.(int))
							if tmp.(bool) {
								tmp = false
							} else if tail == nil {
								join(fail, nil)
							} else {
								join(fail, tail)
							}
							return nil
						}
					}))

					if tmp != nil {
						tmp = false
						return
					}
				} else if lhs == Empty || rhs == Empty {
					// We can only proceed if both sides have resolved.
					Trace("One side EMPTY", lhs, rhs)
					return
				} else {
					Trace("Both sides not EMPTY", lhs, rhs)
					step = right(fromRight(lhs).(Fn1)(fromRight(rhs)))
					head._3 = step
				}
			case "ParAlt":
				lhs = head._1.(*AbstractAff)._3
				rhs = head._2.(*AbstractAff)._3
				// We can only proceed if both have resolved or we have a success
				if lhs == Empty && isLeft(rhs).(bool) || rhs == Empty && isLeft(lhs).(bool) {
					return
				}
				// If both sides resolve with an error, we should continue with the
				// first error
				if lhs != Empty && isLeft(lhs).(bool) && rhs != Empty && isLeft(rhs).(bool) {
					var tmpStep Any
					if step == lhs {
						tmpStep = rhs
					} else {
						tmpStep = lhs
					}
					fail = &tmpStep
					step = nil
					head._3 = fail
				} else {
					head._3 = step
					tmp = true
					kid = killID
					killID++
					// Once a side has resolved, we need to cancel the side that is still
					// pending before we can continue.
					var headArg Any
					if &step == &lhs {
						headArg = head._2
					} else {
						headArg = head._1
					}
					kills.Store(kid.(int), kill(early.(error), headArg.(*AbstractAff), func(unused Any) Any {
						return func() Any {
							kills.Delete(kid.(int))
							if tmp.(bool) {
								tmp = false
							} else if tail == nil {
								join(step, nil)
							} else {
								join(step, tail)
							}
							return nil
						}
					}))

					if tmp.(bool) {
						tmp = false
						return
					}
				}
			}

			if tail == nil {
				head = nil
			} else {
				head = tail.head.(*AbstractAff)
				tail = tail.tail
			}

		}
	}

	resolve := func(fiber *AbstractAff) Any {
		return func(result Any) Any {
			return func() Any {
				Trace("Resolving fiber ", fiber._1.(int))
				fibers.Delete(fiber._1.(int))
				fiber._3 = result
				join(result, fiber._2.(*Cons))
				return nil
			}
		}
	}

	// Walks the applicative tree, substituting non-applicative nodes with
	// `FORKED` nodes. In this tree, all applicative nodes use the `_3` slot
	// as a mutable slot for memoization. In an unresolved state, the `_3`
	// slot is `EMPTY`. In the cases of `ALT` and `APPLY`, we always walk
	// the left side first, because both operations are left-associative. As
	// we `RETURN` from those branches, we then walk the right side.

	run := func() {
		Trace(Green("Run"))
		status := CONTINUE
		step := par
		var head *AbstractAff = nil
		var tail *Cons = nil
		var tmp Any = nil
		fid := 0

	loop:
		for {
			tmp = nil
			Trace(Green("\n############\nrunPar:\t", statusName(status)))
			printCons(&Cons{head: head, tail: tail})
			// Trace("step:\t", step)
			// Trace(Green("#############\n\n"))
			switch status {
			case CONTINUE:
				switch currentStep := step.(type) {
				case AbstractAff:
					switch currentStep.tag {
					case "ParMap":
						Trace(Green("ParMap"))
						if head != nil {
							tail = &Cons{head: head, tail: tail}
						}
						head = &AbstractAff{tag: "ParMap", _1: currentStep._1, _2: Empty, _3: Empty}
						step = currentStep._2
					case "ParApply":
						Trace(Green("ParApply"))
						if head != nil {
							tail = &Cons{head: head, tail: tail}
						}
						head = &AbstractAff{tag: "ParApply", _1: Empty, _2: currentStep._2, _3: Empty}
						step = currentStep._1
					case "ParAlt":
						Trace(Green("ParAlt"))
						if head != nil {
							tail = &Cons{head: head, tail: tail}
						}
						head = &AbstractAff{tag: "ParAlt", _1: Empty, _2: currentStep._2, _3: Empty}
						step = currentStep._1
					default:
						panic(errors.New("Unexpected case " + currentStep.tag))
					}
				default:
					Trace(Green("default, step: ", currentStep))
					// When we hit a leaf value, we suspend the stack in the `FORKED`.
					// When the fiber resolves, it can bubble back up the tree.
					fid = fiberID
					fiberID++
					status = RETURN
					tmp = step
					step = &AbstractAff{tag: "Forked", _1: fid, _2: &Cons{head: head, tail: tail}, _3: Empty}
					tmp = mkFiber(util, supervisor, tmp)
					tmp.(Fiber)["onComplete"].(func(Dict) Any)(OnComplete{
						"rethrow": false,
						"handler": resolve(step.(*AbstractAff)).(func(Any) Any),
					}).(EffFn)()
					fibers.Store(fid, tmp.(Fiber))
					if supervisor != nil {
						supervisor.register(tmp.(Fiber))
					}
				}
			case RETURN:
				// Terminal case. We are back at the root
				if head == nil {
					break loop
				}
				// If we are done with the right side, we need to continue down the
				// left. Otherwise we should continue up the stack.

				if head._1 == Empty {
					Trace("Head._1 is empty")
					head._1 = step
					status = CONTINUE
					step = head._2
					head._2 = Empty
				} else {
					Trace("Head._1 is not empty")
					head._2 = step
					step = head
					if tail == nil {
						head = nil
					} else {
						head = tail.head.(*AbstractAff)
						tail = tail.tail
					}
				}
			}
		}
		// Keep a reference to the tree root so it can be cancelled.
		root = step

		for fid = 0; fid < fiberID; fid++ {
			Trace(Magenta("Running fiber ", fid))
			Trace("Id", fid)
			fiber, ok := fibers.Load(fid)
			if !ok {
				Trace(fibers.internal)
			} else {
				runFn := fiber.(Dict)["run"].(func() Any)
				// [TODO] How about some real parallelism with `go` here?
				// Well no luck due to "concurrent map writes"
				go runFn()
			}
		}

	}

	// Cancels the entire tree. If there are already subtrees being canceled,
	// we need to first cancel those joins. We will then add fresh joins for
	// all pending branches including those that were in the process of being
	// canceled
	cancel := func(cancelErr error, cb K) Any {
		interrupt = left(cancelErr)
		kills.RWMutex.RLock()
		for innerKillID := range kills.internal {
			innerKillsV, ok := kills.Load(innerKillID)
			innerKills := innerKillsV.(*IntMap)
			innerKills.RWMutex.RLock()
			if ok {
				for innerKillID := range innerKills.internal {
					innerKill, ok := innerKills.Load(innerKillID)
					if ok {
						innerKill.(EffFn)()
					}
				}
			}
			innerKills.RWMutex.RUnlock()
		}
		kills.RWMutex.RUnlock()
		kills = nil
		newKills := kill(cancelErr, root.(*AbstractAff), cb)

		return func(killError Any) Any {
			return Async{asyncFn: func(killCb K) Any {
				return func() Any {
					for _, killFn := range newKills.(map[Any]Any) {
						killFn.(EffFn)()
					}
					return nonCanceler
				}
			}}
		}
	}

	run()

	return func(killError Any) Any {
		return Async{asyncFn: func(k K) Any {
			return func() Any {
				return cancel(killError.(error), k)
			}
		}}
	}

}

func init() {
	exports := Foreign("Effect.Aff")

	// ∷ ∀	 a b. Aff a → (a → Aff b) → Aff b
	exports["_pure"] = func(value Any) Any {
		return Pure{Pure: true, value: value}
	}
	// ∷ ∀ a. Error → Aff a
	exports["_throwError"] = func(e_ Any) Any {
		e := e_.(error)
		return Throw{Throw: true, err: e}
	}
	// ∷ ∀ a. Aff a → (Error → Aff a) → Aff a
	exports["_catchError"] = func(aff Any) Any {
		return func(k Any) Any {
			return Catch{aff: aff, errorToAff: k.(func(Any) Any)}
		}
	}
	// ∷ ∀ a b. (a → b) → Aff a → Aff b
	exports["_map"] = func(f_ Any) Any {
		Trace(Yellow("map"))
		f := f_.(func(Any) Any)
		return func(aff Any) Any {
			switch a := aff.(type) {
			case Pure:
				return Pure{Pure: true, value: f(a.value)}
			default:
				return Bind{
					Bind:   true,
					affOfB: aff,
					bToAff: func(b Any) Any { return Pure{Pure: true, value: f(b)} }}
			}
		}
	}

	// ∷ ∀ a b. Aff a → (a → Aff b) → Aff b
	exports["_bind"] = func(aff Any) Any {
		Trace(Yellow("bind"))
		return func(f_ Any) Any {
			f := f_.(func(Any) Any)
			bToAff := func(b Any) Any { return f(b) }
			return Bind{affOfB: aff, bToAff: bToAff}
		}
	}

	exports["_fork"] = func(immediate Any) Any {
		return func(aff Any) Any {
			return Fork{
				immediate: immediate.(bool),
				affOfB:    aff,
			}
		}
	}

	// ∀ a. Effect a → Aff a
	exports["_liftEffect"] = func(effect_ Any) Any {
		effect := effect_.(func() Any)
		return Sync{Sync: true, eff: effect}
	}

	exports["_parAffMap"] = func(f Any) Any {
		return func(aff Any) Any {
			return AbstractAff{tag: "ParMap", _1: f, _2: aff}
		}
	}

	exports["_parAffApply"] = func(aff1 Any) Any {
		return func(aff2 Any) Any {
			return AbstractAff{tag: "ParApply", _1: aff1, _2: aff2}
		}
	}

	exports["_parAffAlt"] = func(aff1 Any) Any {
		return func(aff2 Any) Any {
			return AbstractAff{tag: "ParAlt", _1: aff1, _2: aff2}
		}
	}

	// ∷ ∀ a. Fn.Fn2 FFIUtil (Aff a) (Effect (Fiber a))
	exports["_makeFiber"] = func(util Any, aff Any) Any {
		return func() Any {
			return mkFiber(util, nil, aff)
		}
	}

	// ∷ ∀ a. Fn.Fn2 FFIUtil (Aff a) (Effect (Fiber a))
	exports["_makeSupervisedFiber"] = func(util Any, aff Any) Any {
		return func() Any {
			supervisor := mkSupervisor(util)
			return Dict{
				"fiber":      mkFiber(util, &supervisor, aff),
				"supervisor": supervisor,
			}
		}
	}

	// Fn.Fn3 Error Supervisor (Effect Unit) (Effect Canceler)
	exports["_killAll"] = func(err_ Any, supervisor_ Any, cb_ Any) Any {
		supervisor := supervisor_.(Supervisor)
		cb := cb_.(func() Any)
		err := err_.(error)
		return supervisor.killAll(err, cb)
	}

	// ∀ a. Fn.Fn2 (Unit → Either a Unit) Number (Aff Unit)
	exports["_delay"] = (func() Any {

		setDelay := func(duration time.Duration, k func()) *time.Timer {
			return time.AfterFunc(duration, k)
		}

		clearDelay := func(timer *time.Timer) {
			if timer != nil {
				if !timer.Stop() {
					<-timer.C
				}
			}
		}

		return func(right_ Any, millis_ Any) Any {
			right := right_.(func(Any) Any)
			var millis int = int(millis_.(float64))
			return Async{
				asyncFn: func(cb K) Any {
					return func() Any {
						duration := time.Duration(millis) * time.Millisecond
						k := Apply(cb, right(nil))
						t1 := time.Now()
						timer := setDelay(duration, func() {
							Trace("Done after: ", time.Now().Sub(t1))
							k.(func() Any)()
						})
						return func(err Any) Any {
							Trace(Magenta("Almost canceling delay"))
							return Sync{eff: func() Any {
								// Trace(Magenta("Canceling delay"))
								clearDelay(timer)
								return right(nil)
							}}
						}
					}
				},
			}
		}
	})()

	exports["makeAff"] = func(asyncFn Any) Any {
		Trace("I'm making an aff")
		return Async{asyncFn: asyncFn}
	}

	exports["generalBracket"] = func(acquire Any) Any {
		return func(options Any) Any {
			bracketConditions := options.(Dict)
			return func(k_ Any) Any {
				k := k_.(func(Any) Any)
				return Bracket{
					acquire:           acquire,
					bracketConditions: bracketConditions,
					k:                 k,
				}
			}
		}
	}

	// ParAff ~> Aff
	exports["_sequential"] = func(aff Any) Any {
		return Sequential{parAff: aff}
	}

}

type IntMap struct {
	sync.RWMutex
	internal map[int]Any
}

func NewRegularIntMap() *IntMap {
	return &IntMap{
		internal: make(map[int]Any),
	}
}

func (rm *IntMap) Load(key int) (value Any, ok bool) {
	rm.RLock()
	result, ok := rm.internal[key]
	rm.RUnlock()
	return result, ok
}

func (rm *IntMap) Delete(key int) {
	Trace(Red("Deleting key ", key))
	rm.Lock()
	delete(rm.internal, key)
	rm.Unlock()
}

func (rm *IntMap) Store(key int, value Any) {
	Trace(Green("Storing key ", key))
	rm.Lock()
	rm.internal[key] = value
	rm.Unlock()
}
