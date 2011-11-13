
#include "process.h"

#include "def.h"

#include <stdlib.h>
#include <string.h>

int process(const struct CITY *cities_, bool *partnerships, int num_cities,
	int num_partnerships)
{
	struct CITY *cities = calloc(num_cities, sizeof(struct CITY)); 

	struct PARTNERSHIP *results = 
		calloc(num_partnerships, sizeof(struct PARTNERSHIP));
	int num_results = 0;
		// Ergebnisstack

	bool *processed = calloc(num_cities, sizeof(bool));

	bool processed_all;
	bool possible;

	int i;
	int k;
		
#define PARTNERSHIP(c1, c2) (partnerships[(c1) + num_cities * (c2)])
	// Makro zur Erleichterung des Zugriffs auf das Partnerschaftsarray

#define PUSH_RESULT(h, g, c) \
do { \
	struct PARTNERSHIP partnership; \
	partnership.host = h; \
	partnership.guest = g; \
	partnership.certain = c; \
	results[num_results++] = partnership; \
	cities[h].max_festivals--; \
	cities[h].num_partnerships--; \
	cities[g].num_partnerships--; \
	PARTNERSHIP(h, g) = 0; \
	PARTNERSHIP(g, h) = 0; \
} while(0)
	// Makro, das im Falle eines gefundenen Ergebnisses aufgerufen
	// um alle notwendigen Aktionen auszuführen:
	// 1) Das Ergebnis auf den Ergebnisstack pushen
	// 2) Die Anzahl der verbleibenden Feste und zu feiernden
	//        Partnerschaften anpassen
	// 3) Das Flag für die Partnerschaft auf 0 setzen

	memcpy(cities, cities_, sizeof(struct CITY) * num_cities);

	while(1)
	{
		possible = 1;
			// Gibt an, ob man sich in einer Sackgasse befindet
		processed_all = 0;
			// Gibt an, ob es keine trivialen Schlussfolgerungen
			// mehr möglich sind, also ob mit Raten fortgefahren
			// werden soll

		while(!processed_all && possible)
		{
			processed_all = 1;
			for(i = 0; i < num_cities && possible; i++)
			{
				if(cities[i].max_festivals < 0)
				{
					processed_all = 0;
					possible = 0;
					break;
				}

				if(processed[i])
					// Wenn die Stadt als fertig gekennzeichnet
					// wurde, kann sie übersprungen werden
					continue;

				if(cities[i].max_festivals == 0 
					&& cities[i].num_partnerships > 0)
					// Wenn eine Stadt keine Feste mehr feiern
					// kann, aber noch offene Partnerschaften
					// hat, dann müssen diese offensichtlich
					// von der anderen Stadt gefeiert werden
					for(k = 0; k < num_cities && possible; k++)
						if(PARTNERSHIP(i, k))
						{
							PUSH_RESULT(k, i, 1);
							processed_all = 0;
							processed[k] = 0;

							if(cities[k].max_festivals < 0)
								possible = 0;
						}

				if(cities[i].max_festivals >= cities[i].num_partnerships)
					// Wenn eine Stadt genauso viel oder mehr
					// Feste feiern kann, wie sie offene
					// Partnerschaften hat, dann kann sie
					// offensichtlich alle feiern
					for(k = 0; k < num_cities && possible; k++)
						if(PARTNERSHIP(i, k))
						{
							PUSH_RESULT(i, k, 1);
							processed_all = 0;
							processed[k] = 0;
						}

				processed[i] = 1;
			}
		}

		if(!possible)
		{
			// Aus dem aktuellen Status des Ergebnisstacks kann keine
			// Lösung gefunden werden, daher wird jetzt ein Rollback
			// durchgeführt

			while(--num_results >= 0 && results[num_results].certain)
				// Ergebnisse werden solange vom Stack gepoppt,
				// wie sie sichere Schlussfolgerungen aus den
				// Ergebnissen davor waren.
			{
				int host = results[num_results].host;
				int guest = results[num_results].guest;
				cities[host].max_festivals++;
				cities[host].num_partnerships++;
				cities[guest].num_partnerships++;

				PARTNERSHIP(host, guest) = 1;
				PARTNERSHIP(guest, host) = 1;

				processed[host] = 0;
				processed[guest] = 0;
			}

			if(num_results == -1)
				// Wenn am Ende alle Ergebnisse logische
				// Schlussfolgerungen waren, dann gibt es
				// keine Möglichkeit, das Problem zu lösen
				return 1;
			else
			{
				// Nachdem nun das erste nicht logisch
				// geschlussfolgerte Ergebnis vom Stack
				// gepoppt wurde, wird das Ergebnis
				// umgekehrt und als sicher markiert,
				// damit es beim nächsten Rollback
				// mit-weggenommen wird.
				int host = results[num_results].host;
				int guest = results[num_results].guest;

				results[num_results].host = guest;
				results[num_results].guest = host;
				results[num_results].certain = 1;
				num_results++;

				processed[host] = 0;
				processed[guest] = 0;
			}
		}
		else
		{
			bool done = 1;
				// Gibt an, ob keine offenen Partnerschaften
				// mehr existieren
			for(i = 0; i < num_cities && done; i++)
				for(k = 0; k < num_cities && done; k++)
					if(PARTNERSHIP(i, k))
					{
						done = 0;
						if(cities[i].max_festivals > 0)
							PUSH_RESULT(i, k, 0);
						processed[i] = 0;
						processed[k] = 0;
					}

			if(done)
				break;
		}
	}

	

	for(i = 0; i < num_results; i++)
		PARTNERSHIP(results[i].host, results[i].guest) = 1;

	free(processed);
	free(results);
	free(cities);
		// Aufräumen

	return 0;
}

