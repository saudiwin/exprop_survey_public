# check for DAG dependencies

require(dagitty)
require(ggdag)


# political connections DAG

pol_dag_unident <- dagitty("dag{
                   PC -> PP -> I
                   S -> PC -> I
                   S -> PP -> I
                   PC [exposure]
                   I [outcome]
                   S [unobserved]
                      }")

pol_dag_known_samp <- dagitty("dag{
                   PC -> PP -> I
                   S -> PC -> I
                   S -> PP -> I
                   PC [exposure]
                   I [outcome]
                      }")

pol_dag_manip <- dagitty("dag{
                   PC -> PP -> I
                   PC -> I
                   S -> PP -> I
                   PC [exposure]
                   I [outcome]
                      }")

ggdag(pol_dag_unident) +geom_dag_collider_edges()

ggdag_dseparated(pol_dag_unident,controlling_for = "PP") + geom_dag_collider_edges()
