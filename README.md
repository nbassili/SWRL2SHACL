# SWRL2SPIN
SWRL2SPIN: A tool for transforming SWRL rule bases in OWL ontologies to object-oriented SPIN rules in SWI-Prolog
----------------------------------------------------------------------------------------------------------------
by Dr. Nick Bassiliades, Associate Professor
Department of Informatics, Aristotle University of Thessaloniki, Greece
----------------------------------------------------------------------------------------------------------------
SWRL is a semantic web rule language that combines OWL ontologies with Horn Logic rules of the RuleML family of rule languages, extending the set of OWL axioms to include Horn-like rules. Being supported by the
Protégé ontology editor as well as by popular rule engines and ontology reasoners, such as Jess, Drools and Pellet, SWRL has become a very popular choice for developing rule-based applications on top of
ontologies. However, SWRL being around for more than 10 years now, it is most probable that it will never become a W3C standard; therefore, its scope is difficult to reach out to the industrial world. On the
other hand, SPIN has become a de-facto industry standard to represent SPARQL rules and constraints on Semantic Web models, building on the widespread acceptance of the SPARQL query language for querying and
processing Linked Open Data. In this paper, we argue that the life of existing SWRL rule-based ontology applications can be prolonged by being transformed into SPIN.

To this end, we have developed a prototype tool, called SWRL2SPIN using SWI-Prolog that takes as in-put an OWL ontology with a SWRL rule base and transforms SWRL rules into SPIN rules in the same ontology, taking
into consideration the object-oriented scent of SPIN, i.e. linking rules to the appropriate ontology classes as derived by analyzing the rule conditions. Furthermore, we have optimized the generated SPIN rules.

INSTALLATION
------------
You must install SWI-Prolog from http://www.swi-prolog.org/
We have tested with the latest stable versions 7.6.4 for Windows (both 32-bit and 64-bit).

RUN
---
Double-click swrl2spin.pl from windows explorer or load SWI-Prolog and consult this file.
Then use at the Prolog prompt:

?- swrl2spin(<<"Ontology File">>).

to translate the ontology that contains SWRL rules into an ontology with SPIN rules.

E.g. you can use the three sample files provided:

?-  swrl2spin('university1.owl').

The output is saved into the same directory with the name
'OUT-<Ontology File>'

Thus for the previous example the following file will be generated:

OUT-university1.owl

To test the SPIN rules in TopBraid we must do the following steps:
1) Rename the file to OUT-university1.rdf
2) Load the file from TopBraid and manually import the spif.ttl ontology from the library.

Then press the 'Run Inferences' button.

The provided RDF files are ready to run with TopBraid.

Options
-------
The only currently provided option is subclass(true).
If this option is used then the swrl:ClassAtom is translated into an rdf:type/rdfs:subClassOf* expression instead of a simple rdf:type expression.
In this way, subClassOf reasoning is performed and there is no need to use a reasoner just for that in the rule engine.
(This feature was suggested by Holger Knublauch.)

example use:

?-  swrl2spin('university1.owl',[subclass(true)]).

Documentation
-------------
See technical report 	arXiv:1801.09061 [cs.AI] (https://arxiv.org/abs/1801.09061)

---------------------
For any questions and comments please send an email to Nick Bassiliades at: nbassili@csd.auth.gr
