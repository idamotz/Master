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


