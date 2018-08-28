# SWRL2SHACL
## A tool for transforming SWRL rule bases in OWL ontologies to object-oriented SHACL-SPARQL rules in SWI-Prolog
----------------------------------------------------------------------------------------------------------------
by Dr. Nick Bassiliades, Associate Professor
Department of Informatics, Aristotle University of Thessaloniki, Greece
----------------------------------------------------------------------------------------------------------------
SWRL is a semantic web rule language that combines OWL ontologies with Horn Logic rules of the RuleML family of rule languages, extending the set of OWL axioms to include Horn-like rules. Being supported by the Protégé ontology editor as well as by popular rule engines and ontology reasoners, such as Jess, Drools and Pellet, SWRL has become a very popular choice for developing rule-based applications on top of ontologies. However, SWRL being around for more than 10 years now, it is most probable that it will never become a W3C standard; therefore, its scope is difficult to reach out to the industrial world.

On the other hand, SHACL (Shapes Constraint Language)(https://www.w3.org/TR/shacl/) is a World Wide Web Consortium (W3C) specification for validating graph-based data against a set of conditions. SHACL defines an RDF vocabulary to describe shapes / collections of constraints that apply to a set of nodes. Shapes can be associated with nodes using a flexible target mechanism, e.g. for all instances of a class. One focus area of SHACL is data validation. However, the same principles of describing data patterns in shapes can also be exploited for other purposes. SHACL rules (https://www.w3.org/TR/shacl-af/) build on SHACL to form a light-weight RDF vocabulary for the exchange of rules that can be used to derive inferred RDF triples from existing asserted triples. The SHACL rules feature includes a general framework plus an extension mechanism for specific rule types, one of them being SPARQL rules.

To this end, we have developed a prototype tool, called SWRL2SHACL using SWI-Prolog that takes as input an OWL ontology with a SWRL rule base and transforms SWRL rules into SHACL-SPARQL rules in the same ontology, taking into consideration the object-oriented scent of SHACL, i.e. linking rules to the appropriate ontology classes as derived by analyzing the rule conditions. Furthermore, we are optimizing the generated SHACL rules.

SWRL2SHACL has been derived from the [SWRL2SPIN](https://github.com/nbassili/SWRL2SPIN) tool. Currently its implementation is complicated and non-optimal due to the fact that there has been a quick-and-dirty modification of the SWRL2SPIN tool.

## INSTALLATION

You must install SWI-Prolog from http://www.swi-prolog.org/
We have tested with the latest stable versions 7.6.4 for Windows (both 32-bit and 64-bit).
Then just place all files of the SWRL2SHACL into a folder.

## RUN within the Prolog environment

Double-click `swrl2shacl.pl` from windows explorer or load SWI-Prolog and consult this file.
Then use at the Prolog prompt:

    ?- swrl2shacl(<<"Ontology File">>).

to translate the ontology that contains SWRL rules into an ontology with SPIN rules.

E.g. you can use the three sample files provided:

    ?-  swrl2shacl('university1.owl').

The output is saved into the same directory with the name
`OUT-<Ontology File>`

Thus for the previous example the following file will be generated:

    OUT-university1.owl

## RUN from Windows Command Line

Open cmd.exe and change directory to the folder where the SWRL2SHACL distribution files have been copied.
At the command line type:

    > swrl2shacl.bat <<"Ontology File">>

to translate the ontology that contains SWRL rules into an ontology with SHACL-SPARQL rules.

E.g. you can use the three sample files provided:

    > swrl2shacl.bat university1.owl

The output is saved into the same directory with the name
`OUT-<Ontology File>`

Thus for the previous example the following file will be generated:

    OUT-university1.owl

## TEST SHACL-SPARQL rules

To test the SHACL-SPARQL rules in TopBraid you must do the following steps:
1) Rename the file to `OUT-university1.rdf`
2) Load the file from TopBraid and manually import the `dash.ttl` ontology from the library.

Then press the **Run Inferences** button.

The provided RDF files are ready to run with TopBraid.

### Options

The only currently provided option is `subclass(true)`.
If this option is used then the `swrl:ClassAtom` is translated into an `rdf:type/rdfs:subClassOf*` expression instead of a simple `rdf:type` expression.
In this way, `owl:subClassOf` reasoning is performed and there is no need to use a reasoner just for that in the rule engine.
(This feature was suggested by [Holger Knublauch](https://github.com/HolgerKnublauch).)

example use:

    ?-  swrl2shacl('university1.owl',[subclass(true)]).

From the command line, you can place the options right after the ontology file, e.g.:

    > swrl2shacl.bat university1.owl subclass(true)

## Documentation

* [Technical report for SWRL2SPIN, arXiv:1801.09061 [cs.AI]](https://arxiv.org/abs/1801.09061)

* [Short PowerPoint presentation for SWRL2SPIN](http://intelligence.csd.auth.gr/files/SWRL2SPIN.pptx)

* [SWRL2HACL support for SWRL built-ins](http://intelligence.csd.auth.gr/files/SWRL2SHACL-builtins.pdf)

---------------------
For any questions and comments please send an email to Nick Bassiliades at: nbassili@csd.auth.gr
