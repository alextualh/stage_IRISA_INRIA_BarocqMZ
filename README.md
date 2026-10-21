# *Développement d'un micro-noyau pour RISC-V et validation de ce micro-noyau*

### Généralités :
- Laboratoire : IRISA / Inria Rennes — équipe Epicure (compilation vérifiée, méthodes formelles)
- Encadrants : Sandrine Blazy (professeure, spécialiste de compilation vérifiée), Clément Chavanon (doctorant, projet Barocq)
- Période : stage de 2ᵉ année d'école d'ingénieur — juin, juillet, août 2026
- Code : contient essentiellement du Rocq, de l'assembleur Risc V, du C, du Ocaml, et un langage expérimental développé par Clément Chavanon : le langage Barocq

Présentation du stage et de ce qui a été fait : 

# BarocqMZ — Un micro-noyau RISC-V en langage Barocq, formellement vérifié
Et si l'on pouvait prouver mathématiquement qu'un noyau d'OS est correct ?
C'est le pari des méthodes formelles. Pendant mon stage de 2ᵉ année à l'IRISA/Inria (Rennes), j'ai contribué à rendre cela un peu plus concret à mon échelle : développer un *micro-noyau* de *séparation* en *Barocq*, puis générer son modèle Rocq pour en prouver les propriétés.

## 1. Le problème
Un noyau est le cœur d'un système d'exploitation : il isole les applications, gère la mémoire, ordonnance les tâches, traite les interruptions. Une seule faille peut compromettre toute la sécurité d'une machine.

Les méthodes formelles proposent une réponse radicale : prouver mathématiquement qu'un programme fait ce qu'il doit faire, pour toutes les exécutions possibles. Sur un noyau, l'enjeu est immense — mais la difficulté aussi : il faut un langage adapté, des outils de preuve, et une architecture logicielle qui s'y prête.

C'est exactement le cadre du projet Barocq, porté par l'équipe Epicure (IRISA/Inria) : un langage fonctionnel de bas niveau, compilable vers C, dont les programmes peuvent être transpilés en Rocq (anciennement Coq) pour être formellement vérifiés.

## 2. Ma mission
Reproduire en Barocq un noyau de séparation existant en C — OpenMZ — afin de démontrer l'expressivité du langage sur un cas critique réel, puis générer son modèle Rocq et amorcer sa vérification.

Objectif concret : aboutir à une version stable de BarocqMZ, noyau minimaliste enrichi (ordonnancement, exceptions, interruptions, communication inter-zones), validé sur architecture RISC-V émulée, et intégré dans l'environnement de preuve VeriMZ.

## 3. Ce que j'ai fait
Le stage s'est déroulé en quatre phases :

Prise en main — Lecture du mémoire d'OpenMZ, de la documentation Barocq, compréhension de TinyMZ, apprentissage de Rocq et de VeriMZ.

Implémentation d'un noyau minimal — En Barocq, C et assembleur RISC-V, jusqu'à une première version fonctionnelle.

Développement complet — Ajout de fonctionnalités (ordonnancement, exceptions, interruptions, IPC), résolution de bugs, aboutissant à une version finale stable.

Intégration Rocq et preuves partielles — Génération du modèle Rocq à partir du code final, intégration de la fonction step, adaptation du méta-modèle, amorce de la preuve de correction fonctionnelle.

## 4. Compétences techniques mobilisées
Domaine	Détails
Langages	Barocq (fonctionnel, bas niveau), C, assembleur RISC-V, Rocq/Coq
Méthodes formelles	Génération de modèles Rocq, monades, preuves de propriétés, typage affine
Système bas niveau	Instructions RISC-V, registres, linker script, compilation, QEMU, GDB
Outils	VeriMZ, environnement de preuve Rocq, chaîne de compilation Barocq
Méthode de travail	Journal quotidien, documentation systématique des erreurs, graphes de dépendances, planification par tâches et priorités, revues de code avec l'encadrement

## 5. Résultats
BarocqMZ : version stable et fonctionnelle du noyau, écrite en Barocq, compilée vers C et exécutée sur RISC-V émulé.

Modèle Rocq généré et intégré dans VeriMZ, avec adaptation du méta-modèle et intégration de la fonction step.

Preuves partielles : la formalisation est possible et constitue une base solide pour de futurs travaux de vérification.

Validation approfondie via QEMU et GDB sur l'ensemble des fonctionnalités implémentées.

## 6. Ce que j'en retire
Ce stage m'a conforté dans deux directions qui m'attirent profondément : les méthodes formelles et la programmation système. J'y ai découvert la puissance de la vérification appliquée à du code critique, mais aussi la rigueur nécessaire pour écrire un noyau — deux faces d'une même exigence de correction.

Au-delà du technique, j'ai appris à m'approprier un code inconnu, à organiser un apprentissage autonome, à documenter systématiquement mes erreurs et leurs résolutions, et à calibrer mes échanges avec mes encadrants. Autant de compétences que je souhaite désormais mobiliser dans un cadre professionnel orienté contact et transmission.



