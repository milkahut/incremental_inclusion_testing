open Satwrapper;;

class preprocessorSolverFactory: solverFactory -> object inherit solverFactory
	method description: string
	method identifier: string
	method short_identifier: string
	method copyright: string
	method url: string
	method new_instance: abstractSolver
end