## 27. august
Skrev ned en oversikt over hva som må sjekkes og gjøres (roughly) når man gjør en planendring. Prøvde å begynne å tenke på moves/sykler som oppstår over tid, særlig når man fjerner en move-operasjon fra planen. Begynner å få en følelse av hvordan dependency maps må se ut: 

Names -> validities
FeatureID -> validities of feature, parent group, ftype, name, child group IDs
GroupID -> validities of group, parent feature, gtype, child feature IDs

Må se videre på dette med sykler. Hva er det
minimale tidsintervallet man kan sjekke? Det
minimale antallet features og groups? Hvilke
moves gjør at det er good igjen? 

## Meeting November 19
__Define the scope__

  - What does the scope mean?
  - Give an illustration
  - Define $\Delta$ algorithmically ($\Delta$ is the scope in the rules). 
  - Give the scope for all operations
  - Identify "minimal" scope. Argue that the soundness relies solely on this this scope
  - Two dimentions of scope: Time and space!

Create a rule for each operation. 

Analysis is overapproximating (as per usual for
static analysis) but not too many limitations on
expressiveness. 

Sketch a tree and choose a feature. What can affect me (as a feature)?

__Use examples__

Write down the mappings. 

  - Start with examples that pass, then proceed to illegal changes. 
  
    - Where does the paradox occur? Which parts are affected/need to be checked?
    - Analyse paradox

  - Paradox analysis that is so specific that the user can do something about it

Do it in LaTeX. If I'm stuck, use English to explain the rules, this will help the thesis __and__ the thinking. 

Move between writing and technical according to mood. 

Start by writing motivation, and begin with __addFeature__ (since it's the simplest).

End the day with something left to say (but put a note to remember what it is) $\rightarrow$ Leave a candy!

End the day with something I enjoy. 

## 12. januar
### Har gjort
Har ført inn to SOS-regler i LaTeX. Det er absolutt ikke pent, men jeg tror det er relativt riktig hvis man bøyer reglene for maps som jeg selv har laget:). Måtte endre semantikken for interval maps, fordi det gir mest mening å kunne slå opp hvilke verdier som eksisterer innen et intervall, heller enn å anta at man får ut én eller 0 verdier.

### Står igjen
Tips fra Lars: Trekke ut hårete ting fra reglene, dvs. feks å opprette en ny feature i modellen kan være en ekstern operasjon definert for strukturen. 
Default-map: La verdien man får ut når key ikke finnes være en brukbar ting. Så slipper man feks å dele opp fresh-add og re-add i to regler. 
For å få det pent: Ta bort let-greiene fra over streken. Hvis det ikke er plass under, revurder om syntaksen kan gjøres penere. 

Resten av reglene må finnes på og føres inn, og jeg må skrive en god del sider og lese masse for å komme i mål. Hadde også håpet å få implementert noe, men vet ikke helt om det lar seg gjøre. Det hadde også hjulpet veldig på motivasjonen, som akkurat nå er på bunn.

Helt spesifikt finnes det også et par store problemer som jeg må løse eller skrive at jeg velger å ignorere. Det finnes ingen måte å utvide, redusere eller flytte tiden en feature (eller noe annet) finnes i planen. Det finnes gode grunner til å ha det, så jeg synes det er ganske problematisk. Kanskje det hjelper å se det fra meta-perspektivet igjen? For eksempel vil MOVE remove kunne endre høyresiden av et intervall, og MOVE add vil endre venstresiden. For å flytte hele intervallet må man gjøre to operasjoner, og det kan føre til store problemer. Altså vil ikke meta-perspektivet løse problemet fullstendig, men det kan hjelpe meg med å klargjøre hva jeg selv mener burde være mulig å endre på.  
Noe annet som jeg må ta stilling til på et tidspunkt, er om jeg vil introdusere batch operations. Dette vil da hovedsakelig gjelde i tilfellet når: 

  - to operasjoner påvirker samme ting 
  - å sjekke hver av dem isolert vil føre til paradoks
  - sammen fører de ikke til paradoks

## Meeting 21 January

- Have to make sure that the concept is properly explained; that plans happen in the _future_, and that I'm analysing _modifications_ and not the plans themselves.
- Use examples (make sure to make illustrations, not just syntactic representations)
- Create a small example each time I introduce a concept (for scoping in particular). Maybe one with a paradox and one without a paradox?
- Storytelling is important
- Should implement, nice to have a proof of concept
- Make explicit that this is soundness of _modifications_, not plans (in contrast to previous work)!
- Prioritize semantics and proof until February
- Spend March implementing

## 21. januar notater
Clamp interval (when removing child features etc)

Goal: change $\{[[t_m, t_n] \mapsto \{\texttt{childFeatureID}\} \cup rest]\}$ to
$\{[[t_m, t_n) \mapsto rest] \cup \{[[t_m, t_k) \mapsto  \{\texttt{childFeatureID}\}\}$

1. Find mapping containing time point and child feature
2. Remove mapping/remove child feature from value set
3. Insert child feature at $[t_m, t_k)$

### 2. februar

For TUBS meeting: Think about what they can contribute, what part of the thesis. 

The thesis will need examples. 

Present a big example: Use the tree structure, show what you can detect. Show what the scope of an error is. 

A small and specific change in a huge plan, show how the scope and representation minimize the problem 
